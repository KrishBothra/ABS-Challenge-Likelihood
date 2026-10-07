# =============================================================
# features.R
#
# Inputs: Data/datasets/pitch_split.rds
# Output: Data/datasets/pitch_split_feat.rds
# One coarse location category, original call and game context.
# =============================================================

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dataset_dir <- file.path(data_dir, "datasets")

dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source("functions.R")
csv_dir <- prepare_csv_dir(out, if (length(args) >= 3) args[3] else file.path(dirname(out), "csv"))

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
    "velocity", "break_x", "break_z", "extension",
    # sequence
    "pa_of_game", "pitch_of_pa"
  )

  # Fixed before fitting: the entire ball clears the zone edge by 4 inches.
  # Raw locations are used here only; no coordinates or distances reach the model.
  PLATE_HALF_WIDTH <- 8.5 / 12
  BALL_RADIUS <- 1.5 / 12
  EDGE_CLEARANCE <- 4 / 12
  CLEAR_DISTANCE <- BALL_RADIUS + EDGE_CLEARANCE
  valid_location <- is.finite(df$plate_x) & is.finite(df$plate_z) &
    is.finite(df$sz_top) & is.finite(df$sz_bot) & df$sz_top > df$sz_bot
  location_category <- case_when(
    !valid_location ~ "unknown",
    abs(df$plate_x) <= PLATE_HALF_WIDTH - CLEAR_DISTANCE &
      df$plate_z >= df$sz_bot + CLEAR_DISTANCE &
      df$plate_z <= df$sz_top - CLEAR_DISTANCE ~ "obvious_strike",
    abs(df$plate_x) >= PLATE_HALF_WIDTH + CLEAR_DISTANCE |
      df$plate_z <= df$sz_bot - CLEAR_DISTANCE |
      df$plate_z >= df$sz_top + CLEAR_DISTANCE ~ "obvious_ball",
    TRUE ~ "borderline"
  )

  df |>
    select(all_of(numeric_cols)) |>
    mutate(
      is_top_inning  = as.numeric(df$is_top_inning),
      called_strike  = as.numeric(df$pitch_result == "called_strike"),
      location_category = location_category,
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

forbidden_location <- c("plate_x", "plate_z", "sz_top", "sz_bot", "zone_height",
  "z_normalized", "abs_plate_x", "call_disagreement", "release_x", "release_z",
  "release_angle_x", "release_angle_z")
stopifnot(!any(c(target_cols, id_cols, forbidden_location) %in% feature_cols))
stopifnot(!any(grepl("margin", feature_cols)))
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
