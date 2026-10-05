# =============================================================
# functions.R
#
# Shared calculations used by the stage scripts.
# Sourcing this file does not read data, fit models or write outputs.
# =============================================================

library(dplyr)

# -------------------------------------------------------------
# 1. Input checks and game splits
# -------------------------------------------------------------
required_packages <- c("readxl", "data.table", "Matrix", "glmnet", "xgboost", "ggplot2", "jsonlite", "tidyverse")
check_packages <- function() {
  absent <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Install required packages: ", paste(absent, collapse = ", "))
  if (utils::packageVersion("xgboost") < "3.0.0") stop("XGBoost 3.x is required; run Setup.R")
}

normalize_id <- function(x, width = NULL) {
  x <- sub("\\.0+$", "", as.character(x))
  if (!is.null(width)) x <- ifelse(is.na(x), NA_character_, sprintf(paste0("%0", width, "d"), as.integer(x)))
  x
}

load_data <- function(path, cache_dir) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  fingerprint <- unname(tools::md5sum(path))
  cache <- file.path(cache_dir, paste0(basename(path), ".", fingerprint, ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  d <- if (grepl("\\.xlsx$", path)) {
    as.data.frame(readxl::read_xlsx(path, na = c("", "NA")))
  } else as.data.frame(data.table::fread(path, na.strings = c("", "NA")))
  for (nm in intersect(c("play_id", "venue_id", "pitcher_id", "batter_id", "catcher_id", "umpire_id"), names(d)))
    d[[nm]] <- normalize_id(d[[nm]])
  d$game_id <- normalize_id(d$game_id, 8)
  d$is_top_inning <- as.logical(d$is_top_inning)
  saveRDS(d, cache)
  d
}

make_splits <- function(train, seed, coaching_game = "01103572") {
  set.seed(seed)
  games <- sample(sort(setdiff(unique(train$game_id), coaching_game)))
  n <- length(games)
  labels <- c(rep("fit", floor(n * .6)), rep("tune", floor(n * .2)),
              rep("holdout", n - floor(n * .6) - floor(n * .2)))
  map <- setNames(labels, games)
  split <- unname(map[train$game_id])
  split[train$game_id == coaching_game] <- "coaching"
  stopifnot(!anyNA(split))
  split
}

# -------------------------------------------------------------
# 2. Fitting-set encoding
# -------------------------------------------------------------
fit_encoder <- function(f) {
  cat_cols <- names(f)[vapply(f, is.character, logical(1))]
  num_cols <- setdiff(names(f), cat_cols)
  medians <- vapply(f[num_cols], function(x) {
    m <- median(x[is.finite(x)], na.rm = TRUE)
    if (is.finite(m)) m else 0
  }, numeric(1))
  levels <- lapply(f[cat_cols], function(x) sort(unique(x[!is.na(x)])))
  list(numeric = num_cols, categorical = cat_cols, medians = medians, levels = levels)
}

encode_features <- function(f, encoder) {
  nums <- as.matrix(f[encoder$numeric])
  missing <- !is.finite(nums)
  for (j in seq_len(ncol(nums))) nums[missing[, j], j] <- encoder$medians[j]
  colnames(missing) <- paste0(colnames(nums), "_missing")
  ans <- Matrix::Matrix(cbind(nums, missing * 1), sparse = TRUE)
  for (nm in encoder$categorical) {
    lev <- c(encoder$levels[[nm]], "__UNKNOWN__")
    ix <- match(f[[nm]], lev)
    ix[is.na(ix)] <- length(lev)
    one <- Matrix::sparseMatrix(i = seq_len(nrow(f)), j = ix, x = 1,
                               dims = c(nrow(f), length(lev)),
                               dimnames = list(NULL, paste0(nm, "=", lev)))
    ans <- cbind(ans, one)
  }
  colnames(ans) <- make.names(colnames(ans), unique = TRUE)
  as(ans, "dgCMatrix")
}

# -------------------------------------------------------------
# 3. Prediction and probability diagnostics
# -------------------------------------------------------------
clip_prob <- function(p) pmin(1 - 1e-6, pmax(1e-6, as.numeric(p)))
log_loss <- function(y, p) {
  p <- clip_prob(p)
  -mean(y * log(p) + (1 - y) * log1p(-p))
}
probability_metrics <- function(y, p) {
  p <- clip_prob(p)
  c(log_loss = log_loss(y, p), brier = mean((p-y)^2), observed_rate = mean(y), predicted_rate = mean(p))
}

predict_candidate <- function(model, x) {
  if (model$type == "constant") return(rep(model$p, nrow(x)))
  if (model$type == "call_mapping") return(ifelse(as.numeric(x[, "called_strike"]) == 1, model$strike_p, model$ball_p))
  if (model$type == "ridge") return(clip_prob(predict(model$fit, newx = x, s = model$lambda, type = "response")))
  clip_prob(predict(model$fit, xgboost::xgb.DMatrix(x, nthread = 2)))
}

calibration_table <- function(y, p, task) {
  # Fixed probability bins avoid arbitrary splitting of equal predictions.
  breaks <- if (task == "challenge") c(0, .005, .01, .02, .05, .1, .2, .4, .6, .8, 1) else seq(0, 1, .1)
  bin <- cut(p, breaks = breaks, include.lowest = TRUE)
  do.call(rbind, lapply(levels(bin), function(z) {
    k <- which(bin == z)
    if (!length(k)) return(NULL)
    data.frame(task = task, bin = z, n = length(k), predicted = mean(p[k]), observed = mean(y[k]))
  }))
}

cluster_bootstrap <- function(y, p, baseline, games, seed = 2027, reps = 500) {
  set.seed(seed)
  p <- clip_prob(p)
  baseline <- clip_prob(baseline)
  l <- -(y * log(p) + (1-y) * log1p(-p))
  b <- -(y * log(baseline) + (1-y) * log1p(-baseline))
  totals <- aggregate(cbind(loss = l, baseline_loss = b, n = rep(1, length(y))), list(game = games), sum)
  draws <- replicate(reps, {
    ix <- sample.int(nrow(totals), nrow(totals), replace = TRUE)
    c(log_loss = sum(totals$loss[ix]) / sum(totals$n[ix]),
      improvement = sum(totals$baseline_loss[ix] - totals$loss[ix]) / sum(totals$n[ix]))
  })
  data.frame(metric = rownames(draws), lower = apply(draws, 1, quantile, .025),
             upper = apply(draws, 1, quantile, .975))
}
