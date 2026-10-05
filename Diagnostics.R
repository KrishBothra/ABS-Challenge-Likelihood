# =============================================================
# Diagnostics.R
#
# Read saved validation models. No tuning or refitting here.
# Write holdout metrics, calibration, uncertainty intervals and importance.
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
train <- pitch_split$train
seed <- pitch_split$seed

results <- list(
  challenge = readRDS(file.path(dataset_dir, "model_challenge_validation.rds")),
  success   = readRDS(file.path(dataset_dir, "model_success_validation.rds")),
  source    = readRDS(file.path(dataset_dir, "model_source_validation.rds"))
)

# -------------------------------------------------------------
# Holdout checks and plots
# -------------------------------------------------------------
all_metrics <- do.call(rbind, lapply(results, `[[`, "holdout"))
write.csv(all_metrics, file.path(out, "holdout_metrics.csv"), row.names = FALSE)
all_cal <- list()
all_pred <- list()
ci <- list()
for (task in names(results)) {
  r <- results[[task]]
  ix <- r$holdout_indices
  y <- switch(task, challenge = train$is_challenge[ix], success = train$is_success[ix],
              source = as.numeric(train$challenge_source[ix] == "hitting_team"))
  all_cal[[task]] <- calibration_table(y, r$holdout_predictions, task)
  all_pred[[task]] <- data.frame(task = task, play_id = train$play_id[ix], game_id = train$game_id[ix],
                                actual = y, predicted = r$holdout_predictions)
  baseline <- r$holdout$predicted_rate[r$holdout$model == "constant"][1]
  ci[[task]] <- cbind(task = task, cluster_bootstrap(y, r$holdout_predictions,
                                                   rep(baseline, length(ix)), train$game_id[ix], seed))
}
cal <- do.call(rbind, all_cal)
write.csv(cal, file.path(out, "calibration.csv"), row.names = FALSE)
write.csv(do.call(rbind, all_pred), file.path(out, "holdout_predictions.csv"), row.names = FALSE)
write.csv(do.call(rbind, ci), file.path(out, "holdout_bootstrap_intervals.csv"), row.names = FALSE)
p <- ggplot(cal, aes(predicted, observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey60") +
  geom_point(aes(size = n), color = "#007F7A") +
  facet_wrap(~task, scales = "free") + theme_minimal(base_size = 12) +
  labs(title = "Probability calibration on untouched holdout games",
                x = "Mean predicted probability", y = "Observed frequency", size = "Pitches")
ggsave(file.path(out, "calibration.png"), p, width = 11, height = 4, dpi = 160)
for (task in names(results)) {
  r <- results[[task]]
  if (r$selected$type == "xgb") {
    importance <- as.data.frame(xgboost::xgb.importance(model = r$selected$fit))
    write.csv(importance, file.path(out, paste0(task, "_importance.csv")), row.names = FALSE)
  }
}

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
print(all_metrics)
cat("\nSaved holdout metrics and calibration plots.\n")
