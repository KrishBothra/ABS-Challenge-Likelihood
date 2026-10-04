# Score new data with the saved final models without retraining.
# Rscript scripts/predict.R Data/data-test.xlsx results/models predictions.csv
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/predict.R INPUT MODEL_DIR OUTPUT.csv")
source("R/data.R"); source("R/features.R"); source("R/models.R")
check_packages()
d <- load_data(args[1], file.path(".cache", "data"))
f <- make_features(d)
p <- lapply(c("challenge", "success", "source"), function(task) {
  bundle <- readRDS(file.path(args[2], paste0(task, "_final.rds")))
  predict_candidate(bundle$model, encode_features(f, bundle$encoder))
})
d$p_challenge <- p[[1]]; d$p_success_g_challenge <- p[[2]]
d$challenge_source <- ifelse(p[[3]] >= .5, "hitting_team", "pitching_team")
data.table::fwrite(d, args[3], na = "NA")
cat("Scored", nrow(d), "pitches.\n")
