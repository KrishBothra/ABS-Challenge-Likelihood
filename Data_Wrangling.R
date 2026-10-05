# =============================================================
# Data_Wrangling.R
#
# One row per called pitch. Preserve the labels and original row order.
# Split by GAME, then save the tables for features.R.
# =============================================================

library(tidyverse)
library(readxl)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dataset_dir <- file.path(data_dir, "datasets")

dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
source("functions.R")

check_packages()
SEED <- 2027L
COACHING_GAME <- "01103572"

# -------------------------------------------------------------
# 1. Load the supplied data
# -------------------------------------------------------------
find_input <- function(name) {
  paths <- file.path(data_dir, paste0("data-", name, c(".csv", ".xlsx")))
  available <- paths[file.exists(paths)]
  if (length(available) == 0) stop("Missing data file: ", name)
  available[1]
}

train_path <- find_input("train")
test_path  <- find_input("test")

train <- load_data(train_path, file.path(".cache", "data"))
test  <- load_data(test_path, file.path(".cache", "data"))

# -------------------------------------------------------------
# 2. Check IDs, outcomes and missing values
# -------------------------------------------------------------
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

# -------------------------------------------------------------
# 3. Game-level split
# -------------------------------------------------------------
split <- make_splits(train, SEED, COACHING_GAME)

split_manifest <- train |>
  select(play_id, game_id) |>
  mutate(split = split)

split_summary <- split_manifest |>
  count(split, name = "pitches")

write_csv(split_manifest, file.path(out, "split_manifest.csv"))
write_csv(split_summary, file.path(out, "split_summary.csv"))

# -------------------------------------------------------------
# 4. Save for the feature step
# -------------------------------------------------------------
pitch_split <- list(
  train = train,
  test  = test,
  split = split,
  seed  = SEED,
  coaching_game = COACHING_GAME,
  source_md5 = as.list(tools::md5sum(c(train_path, test_path)))
)

saveRDS(pitch_split, file.path(dataset_dir, "pitch_split.rds"))

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
train |>
  count(is_challenge) |>
  mutate(pct = round(100 * n / sum(n), 2)) |>
  print()

print(split_summary)
cat("\nTrain:", nrow(train), "| Test:", nrow(test), "\n")
cat("Unusual challenge-source labels retained:", nrow(exceptions), "\n")
cat("Saved pitch_split.rds.\n")
