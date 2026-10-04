# Run from the project directory: Rscript Model.R [data_directory] [output_directory]
args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "outputs"
for (file in c("data", "features", "models", "reporting")) source(file.path("R", paste0(file, ".R")))
check_packages()
dir.create(out, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out, "models"), recursive = TRUE, showWarnings = FALSE)
seed <- 2027L
data.table::setDTthreads(2)
find_input <- function(nm) {
  paths <- file.path(data_dir, paste0("data-", nm, c(".csv", ".xlsx")))
  available <- paths[file.exists(paths)]
  if (!length(available)) stop("Missing data file for ", nm, " in ", data_dir)
  available[1]
}
message("Reading and auditing source data")
train_path <- find_input("train"); test_path <- find_input("test")
train <- load_data(train_path, file.path(".cache", "data"))
test <- load_data(test_path, file.path(".cache", "data"))
audit_data(train, test, out)
split <- make_splits(train, seed)
write.csv(data.frame(play_id = train$play_id, game_id = train$game_id, split = split),
          file.path(out, "split_manifest.csv"), row.names = FALSE)
write.csv(as.data.frame(table(split)), file.path(out, "split_summary.csv"), row.names = FALSE)
features <- make_features(train); test_features <- make_features(test)
targets <- list(challenge = train$is_challenge, success = train$is_success,
                source = ifelse(is.na(train$challenge_source), NA_real_, as.numeric(train$challenge_source == "hitting_team")))
results <- list()
for (task in names(targets)) {
  message("Training and evaluating ", task)
  results[[task]] <- fit_task(features, targets[[task]], split, task, out, seed)
  message(task, ": selected ", results[[task]]$selected_name)
  saveRDS(results[[task]], file.path(out, "models", paste0(task, "_validation.rds")))
}
holdout <- save_diagnostics(train, results, out, seed)
coaching <- write_coaching_report(train, features, results, out)
write.csv(coaching, file.path(out, "coaching_summary.csv"), row.names = FALSE)
pred <- list()
for (task in names(targets)) {
  message("Refitting ", task, " on all labeled data")
  final <- refit_task(features, targets[[task]], results[[task]]$selected)
  saveRDS(final, file.path(out, "models", paste0(task, "_final.rds")))
  if (final$model$type == "xgb") xgboost::xgb.save(final$model$fit, file.path(out, "models", paste0(task, ".ubj")))
  pred[[task]] <- predict_candidate(final$model, encode_features(test_features, final$encoder))
}
submission <- test
submission$p_challenge <- pred$challenge
submission$p_success_g_challenge <- pred$success
submission$challenge_source <- ifelse(pred$source >= .5, "hitting_team", "pitching_team")
stopifnot(identical(submission$play_id, test$play_id), nrow(submission) == nrow(test))
for (nm in c("p_challenge", "p_success_g_challenge"))
  stopifnot(all(is.finite(submission[[nm]])), all(submission[[nm]] > 0 & submission[[nm]] < 1))
unchanged <- setdiff(names(test), c("p_challenge", "p_success_g_challenge", "challenge_source"))
stopifnot(identical(submission[unchanged], test[unchanged]))
data.table::fwrite(submission, file.path(out, "data-test-predictions.csv"), na = "NA")
data.table::fwrite(data.frame(play_id = test$play_id, p_hitting_team_g_challenge = pred$source),
                   file.path(out, "source_probabilities.csv"))
manifest <- list(seed = seed, split = "60/20/20 by game; coaching game reserved", coaching_game = "01103572",
                 training_rows = nrow(train), test_rows = nrow(test),
                 source_md5 = as.list(tools::md5sum(c(train_path, test_path))),
                 selected_models = lapply(results, function(r) list(name = r$selected_name,
                   lambda = r$selected$lambda, rounds = r$selected$rounds)),
                 prediction_checks = list(row_order = TRUE, unchanged_inputs = TRUE, complete_probabilities = TRUE),
                 package_versions = as.list(vapply(required_packages, function(p) as.character(utils::packageVersion(p)), character(1))))
jsonlite::write_json(manifest, file.path(out, "run_manifest.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
print(holdout)
message("Complete. Predictions and diagnostics saved to ", normalizePath(out))
