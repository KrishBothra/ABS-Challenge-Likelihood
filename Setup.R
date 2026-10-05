# =============================================================
# Setup.R
#
# Install missing packages once before running the project.
# =============================================================


packages <- c("readxl", "data.table", "Matrix", "glmnet", "xgboost", "ggplot2", "jsonlite", "tidyverse")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (packageVersion("xgboost") < "3.0.0") stop("This project uses the XGBoost 3.x R API; upgrade xgboost.")
cat("Dependencies available. See results/session-info.txt for the versions used in the reference run.\n")
