source("functions.R")

# Load the feature function without running the stage's file I/O.
feature_expressions <- parse("features.R")
for (expression in feature_expressions) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("make_features"))) {
    eval(expression)
  }
}

stopifnot(abs(log_loss(c(0, 1), c(.5, .5)) - log(2)) < 1e-10)
stopifnot(is.finite(log_loss(c(0, 1), c(1, 0))))
f <- data.frame(plate_x = c(0, NA, 2), called_strike = c(0, 1, 0),
                catcher = c("A", "B", "A"), stringsAsFactors = FALSE)
encoder <- fit_encoder(f[1:2, ])
x <- encode_features(f, encoder)
new <- f[3, ]; new$catcher <- "never_seen"
xn <- encode_features(new, encoder)
stopifnot(identical(colnames(x), colnames(xn)), all(is.finite(x@x)))
stopifnot(as.numeric(xn[, grepl("UNKNOWN", colnames(xn))]) == 1)
stopifnot(as.numeric(x[2, "plate_x_missing"]) == 1, as.numeric(x[2, "plate_x"]) == 0)
stopifnot(identical(normalize_id(c(1103572, 23411069), 8), c("01103572", "23411069")))
d <- data.frame(game_id = rep(sprintf("%08d", 1:30), each = 4))
d <- rbind(d, data.frame(game_id = rep("01103572", 3)))
s <- make_splits(d, 2027)
stopifnot(all(vapply(split(s, d$game_id), function(z) length(unique(z)) == 1, logical(1))))
stopifnot(all(s[d$game_id == "01103572"] == "coaching"))
stopifnot(identical(s, make_splits(d, 2027)))
code <- paste(deparse(body(make_features)), collapse = " ")
stopifnot(!grepl("is_challenge|is_success|challenge_source", code))
cat("PASS: probability loss, unseen categories, missing values, ID normalization, grouped splits, feature target exclusion.\n")

# Exercise the installed XGBoost API and serialized inference end to end.
set.seed(2027)
tiny_x <- Matrix::Matrix(matrix(rnorm(800), ncol = 4), sparse = TRUE)
colnames(tiny_x) <- paste0("x", 1:4)
tiny_y <- as.numeric(tiny_x[, 1] > 0)
dm <- xgboost::xgb.DMatrix(tiny_x, label = tiny_y, nthread = 2)
booster <- xgboost::xgb.train(list(objective = "binary:logistic", eval_metric = "logloss", nthread = 2),
                             dm, 5, evals = list(tune = dm), verbose = 0)
stopifnot(nrow(attr(booster, "evaluation_log")) == 5)
tmp <- tempfile(fileext = ".rds")
saveRDS(list(type = "xgb", fit = booster), tmp)
restored <- readRDS(tmp)
stopifnot(isTRUE(all.equal(predict_candidate(restored, tiny_x), as.numeric(predict(booster, dm)), tolerance = 1e-6)))
unlink(tmp)
cat("PASS: XGBoost evaluation history and saved-model inference.\n")


# Coarse geometry and output migration regression checks.
nms <- c("balls", "strikes", "outs", "inning", "pre_score_home", "pre_score_away",
  "velocity", "break_x", "break_z", "extension", "pa_of_game", "pitch_of_pa")
d <- as.data.frame(setNames(rep(list(rep(0, 9)), length(nms)), nms))
d$is_top_inning <- TRUE; d$pitch_result <- "ball"
for (nm in c("pitcher_hand", "batter_hand", "venue_id", "catcher_id", "umpire_id")) d[[nm]] <- "A"
d$sz_bot <- 1.5; d$sz_top <- 3.5
d$plate_x <- c(0, .25-1e-8, .25+1e-8, 14/12-1e-8, 14/12+1e-8, -14/12-1e-8, 0, 0, NA)
d$plate_z <- c(rep(2.5, 6), 1.5-5.5/12-1e-8, 3.5+5.5/12+1e-8, 2.5)
f <- make_features(d)
stopifnot(identical(f$location_category, c("obvious_strike", "obvious_strike", "borderline",
  "borderline", "obvious_ball", "obvious_ball", "obvious_ball", "obvious_ball", "unknown")))
stopifnot(!any(grepl("margin|plate_|sz_|release_|disagreement|normalized|zone_height", names(f))))
d$sz_top[1] <- 1
stopifnot(make_features(d)$location_category[1] == "unknown")
root <- tempfile(); dir.create(root)
out <- file.path(root, "results"); dir.create(out)
writeLines("x\n1", file.path(out, "a.csv"))
writeLines("chart", file.path(out, "chart.png"))
cs <- prepare_csv_dir(out)
stopifnot(file.exists(file.path(cs, "a.csv")), !file.exists(file.path(out, "a.csv")),
          file.exists(file.path(out, "chart.png")))
stopifnot(identical(prepare_csv_dir(out), cs))
writeLines("x\n2", file.path(out, "a.csv"))
stopifnot(inherits(try(prepare_csv_dir(out), silent = TRUE), "try-error"))
stopifnot(readLines(file.path(out, "a.csv"))[2] == "2", readLines(file.path(cs, "a.csv"))[2] == "1")
unlink(root, recursive = TRUE)
cat("PASS: whole-ball clearance boundaries, excluded location inputs, safe CSV migration.\n")

source("tests/coaching_report_checks.R")
