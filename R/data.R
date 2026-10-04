required_packages <- c("readxl", "data.table", "Matrix", "glmnet", "xgboost", "ggplot2", "jsonlite")
check_packages <- function() {
  absent <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) stop("Install required packages: ", paste(absent, collapse = ", "))
  if (utils::packageVersion("xgboost") < "3.0.0") stop("XGBoost 3.x is required; run scripts/install_dependencies.R")
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

audit_data <- function(train, test, out) {
  common <- c("play_id", "game_id", "pitch_result", "plate_x", "plate_z", "sz_top", "sz_bot")
  stopifnot(all(common %in% names(train)), all(common %in% names(test)))
  stopifnot(!anyNA(train$play_id), !anyNA(test$play_id), !anyNA(train$game_id), !anyNA(test$game_id))
  stopifnot(!anyDuplicated(train$play_id), !anyDuplicated(test$play_id))
  stopifnot(!length(intersect(train$play_id, test$play_id)), !length(intersect(train$game_id, test$game_id)))
  stopifnot(all(train$is_challenge %in% 0:1))
  ch <- train$is_challenge == 1
  stopifnot(all(train$is_success[ch] %in% 0:1), all(is.na(train$is_success[!ch])))
  stopifnot(all(is.na(train$challenge_source[!ch])))
  stopifnot(all(train$challenge_source[ch] %in% c("hitting_team", "pitching_team")))
  for (d in list(train, test)) {
    stopifnot(all(d$pitch_result %in% c("ball", "called_strike")))
    stopifnot(all(d$sz_top > d$sz_bot, na.rm = TRUE))
    stopifnot(all(d$balls %in% 0:3), all(d$strikes %in% 0:2), all(d$outs %in% 0:2))
  }
  stopifnot(all(is.na(test$p_challenge)), all(is.na(test$p_success_g_challenge)), all(is.na(test$challenge_source)))
  mapping <- ifelse(train$pitch_result[ch] == "ball", "pitching_team", "hitting_team")
  exceptions <- train[which(ch)[mapping != train$challenge_source[ch]], , drop = FALSE]
  write.csv(exceptions, file.path(out, "challenge_source_exceptions.csv"), row.names = FALSE)
  missing <- do.call(rbind, lapply(c("train", "test"), function(nm) {
    d <- if (nm == "train") train else test
    data.frame(dataset = nm, column = names(d), missing = colSums(is.na(d)), rows = nrow(d))
  }))
  write.csv(missing, file.path(out, "missingness.csv"), row.names = FALSE)
  summary <- data.frame(dataset = c("train", "test"), rows = c(nrow(train), nrow(test)),
                        games = c(length(unique(train$game_id)), length(unique(test$game_id))),
                        columns = c(ncol(train), ncol(test)))
  write.csv(summary, file.path(out, "data_summary.csv"), row.names = FALSE)
  write.csv(as.data.frame(table(train$pitch_result, train$challenge_source, useNA = "ifany")),
            file.path(out, "challenge_source_check.csv"), row.names = FALSE)
  invisible(TRUE)
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
