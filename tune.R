# =============================================================
# tune.R
#
# Compare constant, ridge and boosted-tree models on tuning games.
# Freeze each model choice before scoring the holdout.
# Saved validation models are also used for the coaching game.
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
split <- pitch_split$split
SEED <- pitch_split$seed

stopifnot(identical(train$play_id, pitch_split_feat$train_play_id))
stopifnot(identical(split, pitch_split_feat$split))

# -------------------------------------------------------------
# 0. Shared tuning calculation
# -------------------------------------------------------------
fit_task <- function(features, y, split, task, out, seed = 2027, max_rounds = 700L) {
  valid <- !is.na(y)
  a <- which(split == "fit" & valid)
  b <- which(split == "tune" & valid)
  h <- which(split == "holdout" & valid)
  stopifnot(length(unique(y[a])) == 2, length(unique(y[b])) == 2, length(unique(y[h])) == 2)
  encoder <- fit_encoder(features[a, , drop = FALSE])
  x <- encode_features(features, encoder)
  candidates <- list(constant = list(type = "constant", p = mean(y[a])))
  if (task == "source") {
    z <- features$called_strike[a]
    candidates$call_mapping <- list(type = "call_mapping",
      strike_p = (sum(y[a][z == 1]) + 1) / (sum(z == 1) + 2),
      ball_p = (sum(y[a][z == 0]) + 1) / (sum(z == 0) + 2))
  }
  lambdas <- 10^seq(-1, -5, length.out = 20)
  ridge <- glmnet::glmnet(x[a, ], y[a], family = "binomial", alpha = 0, lambda = lambdas, standardize = TRUE)
  rp <- predict(ridge, newx = x[b, ], type = "response")
  best_lambda <- lambdas[which.min(apply(rp, 2, function(p) log_loss(y[b], p)))]
  candidates$ridge <- list(type = "ridge", fit = ridge, lambda = best_lambda)
  tuning <- list(data.frame(model = "ridge", parameter = lambdas,
                           log_loss = apply(rp, 2, function(p) log_loss(y[b], p))))
  dtrain <- xgboost::xgb.DMatrix(x[a, ], label = y[a], nthread = 2)
  dtune <- xgboost::xgb.DMatrix(x[b, ], label = y[b], nthread = 2)
  for (depth in c(3L, 5L)) {
    message(task, ": fitting boosted trees, depth ", depth)
    params <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
                   max_depth = depth, eta = .04, min_child_weight = 8, subsample = .85,
                   colsample_bytree = .85, lambda = 5, nthread = 2, seed = seed)
    trial <- xgboost::xgb.train(params, dtrain, nrounds = max_rounds, evals = list(tune = dtune),
                              early_stopping_rounds = 50, verbose = 0)
    history <- as.data.frame(attr(trial, "evaluation_log"))
    stopifnot(nrow(history) > 0, "tune_logloss" %in% names(history))
    rounds <- which.min(history$tune_logloss)
    # Refit the exact selected round count instead of relying on version-specific prediction defaults.
    fit <- xgboost::xgb.train(params, dtrain, nrounds = rounds, verbose = 0)
    candidates[[paste0("xgb_depth", depth)]] <- list(type = "xgb", fit = fit, params = params, rounds = rounds)
    history$model <- paste0("xgb_depth", depth)
    write.csv(history, file.path(csv_dir, paste0(task, "_depth", depth, "_learning_curve.csv")), row.names = FALSE)
  }
  tune_metrics <- do.call(rbind, lapply(names(candidates), function(nm) {
    data.frame(task = task, model = nm, t(probability_metrics(y[b], predict_candidate(candidates[[nm]], x[b, ]))))
  }))
  selected_name <- tune_metrics$model[which.min(tune_metrics$log_loss)]
  selected <- candidates[[selected_name]]
  write.csv(tune_metrics, file.path(csv_dir, paste0(task, "_tuning.csv")), row.names = FALSE)
  write.csv(tuning[[1]], file.path(csv_dir, paste0(task, "_ridge_tuning.csv")), row.names = FALSE)
  # Holdout is scored only after the model choice is frozen on tune games.
  holdout <- do.call(rbind, lapply(c("constant", selected_name), function(nm) {
    data.frame(task = task, model = nm, t(probability_metrics(y[h], predict_candidate(candidates[[nm]], x[h, ]))))
  }))
  holdout <- unique(holdout)
  hp <- predict_candidate(selected, x[h, ])
  list(selected = selected, selected_name = selected_name, encoder = encoder,
       holdout = holdout, holdout_indices = h, holdout_predictions = hp,
       tune = tune_metrics, validation_model = selected)
}


# -------------------------------------------------------------
# (A) Any challenge
# -------------------------------------------------------------
tune_challenge <- fit_task(
  features = train_feat,
  y = train$is_challenge,
  split = split,
  task = "challenge",
  out = out,
  seed = SEED
)

saveRDS(tune_challenge, file.path(dataset_dir, "model_challenge_validation.rds"))

# -------------------------------------------------------------
# (B) Success given a challenge
# -------------------------------------------------------------
tune_success <- fit_task(
  features = train_feat,
  y = train$is_success,
  split = split,
  task = "success",
  out = out,
  seed = SEED
)

saveRDS(tune_success, file.path(dataset_dir, "model_success_validation.rds"))

# -------------------------------------------------------------
# (C) Challenging team
# -------------------------------------------------------------
tune_source <- fit_task(
  features = train_feat,
  y = ifelse(is.na(train$challenge_source), NA_real_,
         as.numeric(train$challenge_source == "hitting_team")),
  split = split,
  task = "source",
  out = out,
  seed = SEED
)

saveRDS(tune_source, file.path(dataset_dir, "model_source_validation.rds"))

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
tuning_results <- bind_rows(
  tune_challenge$tune,
  tune_success$tune,
  tune_source$tune
)

print(tuning_results)
cat("\nSelected challenge model:", tune_challenge$selected_name, "\n")
cat("Selected success model:  ", tune_success$selected_name, "\n")
cat("Selected source model:   ", tune_source$selected_name, "\n")
cat("\nSaved model_challenge_validation, model_success_validation and model_source_validation.\n")

