# =============================================================
# Run_All.R
#
# Run the stage scripts in order.
# Rscript Run_All.R [data_directory] [output_directory]
# =============================================================


stages <- c(
  "Data_Wrangling.R",
  "features.R",
  "tune.R",
  "Model.R",
  "Predict.R",
  "Diagnostics.R"
  #, "Catcher_Report.R"
)

for (stage in stages) {
  cat("\n=====================================================\n")
  cat("Running", stage, "\n")
  cat("=====================================================\n")
  # Each stage reads its own RDS inputs, just as in a fresh R session.
  sys.source(stage, envir = new.env(parent = globalenv()))
}

cat("\nAll stages complete.\n")
