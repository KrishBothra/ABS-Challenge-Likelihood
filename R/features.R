# Geometry is a modeling proxy, not a declaration of the official ABS boundary.
# A nominal baseball radius of 1.45 inches expands the 17-inch plate rectangle.
make_features <- function(d) {
  numeric_cols <- c("balls", "strikes", "outs", "inning", "pre_score_home", "pre_score_away",
                    "velocity", "break_x", "break_z", "release_x", "release_z", "release_angle_x",
                    "release_angle_z", "extension", "plate_x", "plate_z", "sz_top", "sz_bot",
                    "pa_of_game", "pitch_of_pa")
  f <- d[, numeric_cols, drop = FALSE]
  f$is_top_inning <- as.numeric(d$is_top_inning)
  f$called_strike <- as.numeric(d$pitch_result == "called_strike")
  f$zone_height <- d$sz_top - d$sz_bot
  f$z_normalized <- (d$plate_z - d$sz_bot) / f$zone_height
  f$abs_plate_x <- abs(d$plate_x)
  f$horizontal_margin <- 8.5 / 12 - abs(d$plate_x)
  f$bottom_margin <- d$plate_z - d$sz_bot
  f$top_margin <- d$sz_top - d$plate_z
  f$zone_margin <- pmin(f$horizontal_margin, f$bottom_margin, f$top_margin)
  f$ball_margin <- f$zone_margin + 1.45 / 12
  f$call_disagreement <- ifelse(f$called_strike == 1, -f$ball_margin, f$ball_margin)
  f$abs_margin <- abs(f$ball_margin)
  f$batting_score_diff <- ifelse(d$is_top_inning, d$pre_score_away - d$pre_score_home,
                                d$pre_score_home - d$pre_score_away)
  f$close_game <- as.numeric(abs(f$batting_score_diff) <= 2)
  f$two_strikes <- as.numeric(d$strikes == 2)
  f$three_balls <- as.numeric(d$balls == 3)
  f$terminal_call <- as.numeric((f$called_strike == 1 & d$strikes == 2) |
                                 (f$called_strike == 0 & d$balls == 3))
  f$count <- paste(d$balls, d$strikes, sep = "-")
  for (nm in c("pitcher_hand", "batter_hand", "venue_id", "catcher_id", "umpire_id")) f[[nm]] <- as.character(d[[nm]])
  # Pitcher/batter identities are omitted to keep the first model compact and reduce memorization.
  f
}

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
