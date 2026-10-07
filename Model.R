# =============================================================
# Model.R
#
# Refit the selected models on all labeled pitches.
# Inputs: pitch_split.rds, pitch_split_feat.rds and model_*_validation.rds.
# Diagnostics are grouped at the end. Run tune.R first.
# =============================================================

library(tidyverse)
library(glmnet)
library(xgboost)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dataset_dir <- file.path(data_dir, "datasets")

dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source("functions.R")
csv_dir <- prepare_csv_dir(out, if (length(args) >= 3) args[3] else file.path(dirname(out), "csv"))

pitch_split <- readRDS(file.path(dataset_dir, "pitch_split.rds"))
pitch_split_feat <- readRDS(file.path(dataset_dir, "pitch_split_feat.rds"))
train <- pitch_split$train
train_feat <- pitch_split_feat$train

stopifnot(identical(train$play_id, pitch_split_feat$train_play_id))

# Shared fitting calculation for the three targets
refit_task <- function(features, y, selected) {
  keep <- which(!is.na(y))
  encoder <- fit_encoder(features[keep, , drop = FALSE])
  x <- encode_features(features[keep, , drop = FALSE], encoder)
  model <- selected
  if (model$type == "constant") model$p <- mean(y[keep])
  if (model$type == "call_mapping") {
    z <- features$called_strike[keep]
    model$strike_p <- (sum(y[keep][z == 1]) + 1) / (sum(z == 1) + 2)
    model$ball_p <- (sum(y[keep][z == 0]) + 1) / (sum(z == 0) + 2)
  }
  if (model$type == "ridge") model$fit <- glmnet::glmnet(x, y[keep], family = "binomial", alpha = 0,
                                                         lambda = model$lambda, standardize = TRUE)
  if (model$type == "xgb") model$fit <- xgboost::xgb.train(model$params,
    xgboost::xgb.DMatrix(x, label = y[keep], nthread = 2), nrounds = model$rounds, verbose = 0)
  list(model = model, encoder = encoder)
}

# -------------------------------------------------------------
# (A) Any challenge
# -------------------------------------------------------------
tune_challenge <- readRDS(file.path(dataset_dir, "model_challenge_validation.rds"))

fit_challenge <- refit_task(
  features = train_feat,
  y = train$is_challenge,
  selected = tune_challenge$selected
)

saveRDS(fit_challenge, file.path(dataset_dir, "model_challenge.rds"))
if (fit_challenge$model$type == "xgb") {
  xgb.save(fit_challenge$model$fit, file.path(dataset_dir, "model_challenge.ubj"))
}

# -------------------------------------------------------------
# (B) Success given a challenge
# -------------------------------------------------------------
tune_success <- readRDS(file.path(dataset_dir, "model_success_validation.rds"))

fit_success <- refit_task(
  features = train_feat,
  y = train$is_success,
  selected = tune_success$selected
)

saveRDS(fit_success, file.path(dataset_dir, "model_success.rds"))
if (fit_success$model$type == "xgb") {
  xgb.save(fit_success$model$fit, file.path(dataset_dir, "model_success.ubj"))
}

# -------------------------------------------------------------
# (C) Challenging team
# -------------------------------------------------------------
tune_source <- readRDS(file.path(dataset_dir, "model_source_validation.rds"))

fit_source <- refit_task(
  features = train_feat,
  y = ifelse(is.na(train$challenge_source), NA_real_,
         as.numeric(train$challenge_source == "hitting_team")),
  selected = tune_source$selected
)

saveRDS(fit_source, file.path(dataset_dir, "model_source.rds"))
if (fit_source$model$type == "xgb") {
  xgb.save(fit_source$model$fit, file.path(dataset_dir, "model_source.ubj"))
}

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
holdout_results <- bind_rows(
  tune_challenge$holdout,
  tune_success$holdout,
  tune_source$holdout
)

cat("\nHeld-out results for the validation models (before the full-data refit):\n")
print(holdout_results)
cat("\nSaved model_challenge.rds, model_success.rds and model_source.rds.\n")
cat("Next: Predict.R to fill the test predictions.\n")

