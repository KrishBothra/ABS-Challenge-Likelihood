# =============================================================
# Variable_Importance.R
# All-feature split gain for the three final models.
# =============================================================
library(tidyverse)
library(xgboost)
source("functions.R")

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "Data"
out <- if (length(args) >= 2) args[2] else "results"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
csv_dir <- prepare_csv_dir(out, if (length(args) >= 3) args[3] else file.path(dirname(out), "csv"))

# -------------------------------------------------------------
# 1. Group encoded columns under their original feature
# -------------------------------------------------------------
all_importance <- list()
all_encoded <- list()
for (task in c("challenge", "success", "source")) {
  fit <- readRDS(file.path(data_dir, "datasets", paste0("model_", task, ".rds")))
  if (fit$model$type != "xgb") stop("Gain importance requires XGBoost: ", task)
  encoder <- fit$encoder
  original <- c(encoder$numeric, encoder$numeric)
  encoded <- c(encoder$numeric, paste0(encoder$numeric, "_missing"))
  for (nm in encoder$categorical) {
    lev <- c(encoder$levels[[nm]], "__UNKNOWN__")
    original <- c(original, rep(nm, length(lev)))
    encoded <- c(encoded, paste0(nm, "=", lev))
  }
  mapping <- tibble(Feature = make.names(encoded, unique = TRUE), variable = original)
  importance <- as_tibble(xgb.importance(model = fit$model$fit))
  stopifnot(all(importance$Feature %in% mapping$Feature))
  full <- mapping |>
    left_join(importance, by = "Feature") |>
    mutate(across(c(Gain, Cover, Frequency), ~replace_na(.x, 0)), task = task)
  all_encoded[[task]] <- full
  all_importance[[task]] <- full |>
    group_by(task, variable) |>
    summarise(Gain = sum(Gain), .groups = "drop")
}
importance <- bind_rows(all_importance)
write_csv(importance, file.path(csv_dir, "variable_importance_all.csv"))
write_csv(bind_rows(all_encoded), file.path(csv_dir, "variable_importance_encoded.csv"))

# -------------------------------------------------------------
# 2. Show every input feature, including those with zero gain
# -------------------------------------------------------------
ordering <- importance |>
  filter(task == "challenge") |>
  arrange(Gain, variable) |>
  pull(variable)
importance <- importance |>
  mutate(variable = factor(variable, levels = ordering),
    task = factor(task, levels = c("challenge", "success", "source"),
      labels = c("Challenge likelihood", "Overturn if challenged", "Challenging team")),
    label = case_when(Gain == 0 ~ "0%", Gain < .001 ~ "<0.1%",
                      TRUE ~ sprintf("%.1f%%", Gain * 100)))
vip_plot <- ggplot(importance, aes(Gain, variable)) +
  geom_col(fill = "#007F7A", width = .72) +
  geom_text(aes(label = label), hjust = -.1, size = 2.5) +
  facet_wrap(~task, nrow = 1, scales = "free_x") +
  scale_x_continuous(labels = scales::label_percent(),
                     expand = expansion(mult = c(0, .25))) +
  labs(title = "Variable importance: 4-inch whole-ball clearance",
    subtitle = "Refitted models | Obvious category only | Ball center 5.5 inches beyond or inside zone edges",
    x = "Share of total split gain (separate scale for each model)", y = NULL,
    caption = paste("Categorical levels and missing-value indicators are grouped under their original feature.",
      "Gain measures contribution to training splits, not effect direction, causality, or held-out predictive value.",
      "Correlated features can share importance. Zero gain means the feature was not used in a split.", sep = "\n")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    strip.text = element_text(face = "bold"), plot.caption = element_text(hjust = 0),
    plot.margin = margin(15, 25, 15, 15))
if (interactive()) print(vip_plot)
ggsave(file.path(out, "variable_importance_all.png"), vip_plot,
       width = 14, height = 12, dpi = 180, bg = "white")

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
print(importance |> filter(task == "Challenge likelihood") |> arrange(desc(Gain)) |> head(10))
