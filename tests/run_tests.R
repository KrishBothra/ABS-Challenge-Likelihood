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
