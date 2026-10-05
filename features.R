# =============================================================
# features.R
#
# Inputs: Data/datasets/pitch_split.rds
# Output: Data/datasets/pitch_split_feat.rds
# Pitch location, original call and game context only.
# =============================================================

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dataset_dir <- file.path(data_dir, "datasets")

dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source("functions.R")

# -------------------------------------------------------------
# 1. Load the cleaned tables
# -------------------------------------------------------------
pitch_split <- readRDS(file.path(dataset_dir, "pitch_split.rds"))
train <- pitch_split$train
test  <- pitch_split$test

make_features <- function(df) {
  numeric_cols <- c(
    # count and game situation
    "balls", "strikes", "outs", "inning", "pre_score_home", "pre_score_away",
    # pitch movement and release
    "velocity", "break_x", "break_z", "release_x", "release_z",
    "release_angle_x", "release_angle_z", "extension",
    # location and sequence
    "plate_x", "plate_z", "sz_top", "sz_bot", "pa_of_game", "pitch_of_pa"
  )

  # Geometry is a feature proxy, not an exact reconstruction of ABS.
  PLATE_HALF_WIDTH <- 8.5 / 12
  BALL_RADIUS      <- 1.45 / 12

  df |>
    select(all_of(numeric_cols)) |>
    mutate(
      is_top_inning  = as.numeric(df$is_top_inning),
      called_strike  = as.numeric(df$pitch_result == "called_strike"),
      zone_height    = sz_top - sz_bot,
      z_normalized   = (plate_z - sz_bot) / zone_height,
      abs_plate_x    = abs(plate_x),
      horizontal_margin = PLATE_HALF_WIDTH - abs_plate_x,
      bottom_margin = plate_z - sz_bot,
      top_margin    = sz_top - plate_z,
      zone_margin   = pmin(horizontal_margin, bottom_margin, top_margin),
      ball_margin   = zone_margin + BALL_RADIUS,
      call_disagreement = if_else(called_strike == 1, -ball_margin, ball_margin),
      abs_margin    = abs(ball_margin),
      batting_score_diff = if_else(
        df$is_top_inning,
        pre_score_away - pre_score_home,
        pre_score_home - pre_score_away
      ),
      close_game    = as.numeric(abs(batting_score_diff) <= 2),
      two_strikes   = as.numeric(strikes == 2),
      three_balls   = as.numeric(balls == 3),
      terminal_call = as.numeric(
        (called_strike == 1 & strikes == 2) |
          (called_strike == 0 & balls == 3)
      ),
      count = paste(balls, strikes, sep = "-"),
      pitcher_hand = as.character(df$pitcher_hand),
      batter_hand  = as.character(df$batter_hand),
      venue_id     = as.character(df$venue_id),
      catcher_id   = as.character(df$catcher_id),
      umpire_id    = as.character(df$umpire_id)
    ) |>
    as.data.frame()
}

# -------------------------------------------------------------
# 2. Build the features
# -------------------------------------------------------------
# These same saved features are used by tuning, refitting and prediction.
# Column order is preserved because the saved encoders use these names.
train_feat <- make_features(train)
test_feat  <- make_features(test)
feature_cols <- names(train_feat)

target_cols <- c("is_challenge", "is_success", "challenge_source")
id_cols     <- c("play_id", "game_id")

stopifnot(!any(c(target_cols, id_cols) %in% feature_cols))
stopifnot(identical(names(train_feat), names(test_feat)))
stopifnot(nrow(train_feat) == nrow(train), nrow(test_feat) == nrow(test))

# -------------------------------------------------------------
# 3. Save for tuning and modeling
# -------------------------------------------------------------
pitch_split_feat <- list(
  train = train_feat,
  test  = test_feat,
  feature_cols = feature_cols,
  train_play_id = train$play_id,
  test_play_id  = test$play_id,
  split = pitch_split$split
)

saveRDS(pitch_split_feat, file.path(dataset_dir, "pitch_split_feat.rds"))

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
cat("\nFeature columns used:", length(feature_cols), "\n")
print(feature_cols)

train_feat |>
  summarise(across(everything(), ~ round(100 * mean(is.na(.x)), 3))) |>
  pivot_longer(everything(), names_to = "feature", values_to = "pct_na") |>
  filter(pct_na > 0) |>
  arrange(desc(pct_na)) |>
  print(n = Inf)

cat("\nSaved pitch_split_feat.rds.\n")
