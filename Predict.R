# =============================================================
# Predict.R
#
# Load the final models and fill the three requested test columns.
# No retraining. Preserve every test row and non-target value.
# =============================================================

library(tidyverse)
library(xgboost)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dataset_dir <- file.path(data_dir, "datasets")

dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source("functions.R")

pitch_split <- readRDS(file.path(dataset_dir, "pitch_split.rds"))
pitch_split_feat <- readRDS(file.path(dataset_dir, "pitch_split_feat.rds"))
test <- pitch_split$test
test_feat <- pitch_split_feat$test

stopifnot(identical(test$play_id, pitch_split_feat$test_play_id))

# -------------------------------------------------------------
# 1. Load the final models
# -------------------------------------------------------------
fit_challenge <- readRDS(file.path(dataset_dir, "model_challenge.rds"))
fit_success <- readRDS(file.path(dataset_dir, "model_success.rds"))
fit_source <- readRDS(file.path(dataset_dir, "model_source.rds"))

# -------------------------------------------------------------
# 2. Predict each target
# -------------------------------------------------------------
pred_challenge <- predict_candidate(
  fit_challenge$model,
  encode_features(test_feat, fit_challenge$encoder)
)

pred_success <- predict_candidate(
  fit_success$model,
  encode_features(test_feat, fit_success$encoder)
)

pred_source <- predict_candidate(
  fit_source$model,
  encode_features(test_feat, fit_source$encoder)
)

# -------------------------------------------------------------
# 3. Fill and verify the submission
# -------------------------------------------------------------
submission <- test |>
  mutate(
    p_challenge = pred_challenge,
    p_success_g_challenge = pred_success,
    challenge_source = if_else(pred_source >= .5, "hitting_team", "pitching_team")
  )

stopifnot(identical(submission$play_id, test$play_id))
stopifnot(nrow(submission) == nrow(test))
for (name in c("p_challenge", "p_success_g_challenge")) {
  stopifnot(all(is.finite(submission[[name]])))
  stopifnot(all(submission[[name]] > 0 & submission[[name]] < 1))
}
unchanged <- setdiff(names(test), c("p_challenge", "p_success_g_challenge", "challenge_source"))
stopifnot(identical(submission[unchanged], test[unchanged]))

data.table::fwrite(submission, file.path(out, "data-test-predictions.csv"), na = "NA")

source_probabilities <- test |>
  select(play_id) |>
  mutate(p_hitting_team_g_challenge = pred_source)

data.table::fwrite(source_probabilities, file.path(out, "source_probabilities.csv"))

# -------------------------------------------------------------
# 4. Record the run
# -------------------------------------------------------------
results <- list(
  challenge = readRDS(file.path(dataset_dir, "model_challenge_validation.rds")),
  success   = readRDS(file.path(dataset_dir, "model_success_validation.rds")),
  source    = readRDS(file.path(dataset_dir, "model_source_validation.rds"))
)

manifest <- list(
  seed = pitch_split$seed,
  split = "60/20/20 by game; coaching game reserved",
  coaching_game = pitch_split$coaching_game,
  training_rows = nrow(pitch_split$train),
  test_rows = nrow(test),
  source_md5 = pitch_split$source_md5,
  selected_models = lapply(results, function(result) {
    list(name = result$selected_name,
         lambda = result$selected$lambda,
         rounds = result$selected$rounds)
  }),
  prediction_checks = list(row_order = TRUE, unchanged_inputs = TRUE,
                           complete_probabilities = TRUE),
  package_versions = as.list(vapply(required_packages, function(package) {
    as.character(packageVersion(package))
  }, character(1)))
)

jsonlite::write_json(manifest, file.path(out, "run_manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
submission |>
  summarise(
    pitches = n(),
    mean_p_challenge = mean(p_challenge),
    mean_p_success = mean(p_success_g_challenge)
  ) |>
  print()

cat("\nSaved data-test-predictions.csv in original test-row order.\n")
