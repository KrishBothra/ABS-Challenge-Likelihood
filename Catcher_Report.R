# =============================================================
# Catcher_Report.R
#
# Game 01103572, home defense. Read the saved validation models.
# This game was excluded from fitting and model selection.
# Outputs: pitch-level evidence, HTML report and one-page PDF.
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
csv_dir <- prepare_csv_dir(out, if (length(args) >= 3) args[3] else file.path(dirname(out), "csv"))

pitch_split <- readRDS(file.path(dataset_dir, "pitch_split.rds"))
pitch_split_feat <- readRDS(file.path(dataset_dir, "pitch_split_feat.rds"))
train <- pitch_split$train
features <- pitch_split_feat$train
game <- pitch_split$coaching_game

stopifnot(identical(train$play_id, pitch_split_feat$train_play_id))

results <- list(
  challenge = readRDS(file.path(dataset_dir, "model_challenge_validation.rds")),
  success   = readRDS(file.path(dataset_dir, "model_success_validation.rds")),
  source    = readRDS(file.path(dataset_dir, "model_source_validation.rds"))
)

# -------------------------------------------------------------
# 1. Pitch-level review and HTML report
# -------------------------------------------------------------
ix <- which(train$game_id == game & train$is_top_inning)
if (!length(ix)) stop("Coaching game/home defense not found: ", game)
g <- train[ix, , drop = FALSE]
for (task in c("challenge", "success")) {
  r <- results[[task]]
  g[[paste0("p_", task)]] <- predict_candidate(r$validation_model,
    encode_features(features[ix, , drop = FALSE], r$encoder))
}
g <- g |>
  arrange(pa_of_game, pitch_of_pa)
own <- g$is_challenge == 1 & g$challenge_source == "pitching_team"
opp <- g$is_challenge == 1 & g$challenge_source == "hitting_team"
nc <- sum(own)
wins <- sum(g$is_success[own])
losses <- nc - wins
write.csv(g, file.path(csv_dir, "coaching_game_pitches.csv"), row.names = FALSE)
events <- g[g$is_challenge == 1, c("play_id", "inning", "balls", "strikes", "outs", "pitch_result",
                                  "challenge_source", "is_success", "p_success", "plate_x", "plate_z")]
write.csv(events, file.path(csv_dir, "coaching_challenges.csv"), row.names = FALSE)
# Rank review candidates, without pretending unobserved outcomes are known mistakes.
review <- g |>
  filter(is_challenge == 0, pitch_result == "ball") |>
  arrange(desc(p_success)) |>
  slice_head(n = 5)
write.csv(review, file.path(csv_dir, "coaching_review_candidates.csv"), row.names = FALSE)
g$z_normalized <- (g$plate_z - g$sz_bot) / (g$sz_top - g$sz_bot)
g$event <- ifelse(g$is_challenge == 0, "Not challenged",
                  ifelse(g$is_success == 1, "Overturned", "Upheld"))
p <- ggplot(g, aes(plate_x, z_normalized)) +
  annotate("rect", xmin = -8.5/12, xmax = 8.5/12, ymin = 0, ymax = 1,
                    fill = NA, color = "#1C3545", linewidth = .6) +
  geom_point(aes(color = event, shape = pitch_result), alpha = .65, size = 2.2) +
  scale_color_manual(values = c("Not challenged" = "#BAC3CC", "Overturned" = "#007F7A", "Upheld" = "#CB593D")) +
  theme_minimal(base_size = 11) +
  labs(x = "Horizontal location (feet; tracking coordinates)", y = "Height relative to batter zone",
                color = NULL, shape = "Original call") +
  theme(legend.position = "bottom")
ggsave(file.path(out, "coaching_pitch_map.png"), p, width = 7, height = 3.7, dpi = 160)
rows <- if (nrow(events)) paste(apply(events, 1, function(e) sprintf(
  "<tr><td>%s</td><td>%s-%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
  e[["inning"]], e[["balls"]], e[["strikes"]], e[["pitch_result"]],
  e[["challenge_source"]], ifelse(as.numeric(e[["is_success"]]) == 1, "Overturned", "Upheld"))), collapse = "\n") else ""
html <- sprintf('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>ABS catcher review - %s</title>
<style>body{font:14px Arial,sans-serif;color:#17313f;max-width:850px;margin:28px auto;line-height:1.45}h1{font-size:27px;margin-bottom:4px}h2{font-size:17px;border-bottom:1px solid #ccd6db;padding-bottom:4px}.sub{color:#526674}.stats{display:flex;gap:35px;background:#edf5f4;padding:14px;margin:18px 0}.stats b{font-size:25px;display:block;color:#007f7a}table{border-collapse:collapse;width:100%%;font-size:12px}th,td{text-align:left;padding:6px;border-bottom:1px solid #dfe5e8}img{width:100%%;max-height:325px;object-fit:contain}.note{font-size:12px;color:#526674}@page{size:letter;margin:0.5in}@media print{body{margin:0;font-size:11px;max-width:none}.stats{margin:10px 0}h1{font-size:23px}img{max-height:260px}h2{margin:10px 0 5px}p{margin:6px 0}}</style></head>
<body><h1>ABS catcher review</h1><div class="sub">Game %s | Home defense | Catcher %s</div>
<div class="stats"><div><b>%d</b>Called pitches received</div><div><b>%d / %d</b>Defensive challenges won</div><div><b>%d</b>Defensive challenges upheld</div></div>
<h2>What happened</h2><p>The home defense challenged %d calls, overturning %d. The opposing offense challenged %d calls against the home defense, with %d overturned.</p>
<table><thead><tr><th>Inning</th><th>Count</th><th>Original call</th><th>Challenger</th><th>Result</th></tr></thead><tbody>%s</tbody></table>
<h2>Where the calls were</h2><img src="coaching_pitch_map.png" alt="Pitch locations relative to each batter strike zone, with challenge outcomes highlighted">
<h2>Coaching follow-up</h2><p>Review the challenged pitches on video, focusing on the catcher sightline, glove movement, and count. Compare those decisions with the five unchallenged balls in the accompanying review list, ranked by model-estimated reversal probability. These are review candidates, not confirmed missed challenges.</p>
<p class="note">Coverage: called pitches only, top halves of innings. Challenge source identifies the team, so a defensive challenge cannot be attributed uniquely to the catcher rather than the pitcher. The outlined zone is a location reference. Review probabilities come from a model that excluded this game and are learned only from observed challenges; outcomes for unchallenged pitches remain unknown. No causal runs-saved claim is made.</p></body></html>',
  game, game, paste(unique(g$catcher_id), collapse = ", "), nrow(g), wins, nc, losses,
  nc, wins, sum(opp), sum(g$is_success[opp]), rows)
writeLines(html, file.path(out, "catcher_report.html"))

coaching_summary <- tibble(
  game_id = game,
  catcher_id = paste(unique(g$catcher_id), collapse = ","),
  pitches = nrow(g),
  defensive_challenges = nc,
  wins = wins,
  losses = losses
)
write_csv(coaching_summary, file.path(csv_dir, "coaching_summary.csv"))

# -------------------------------------------------------------
# 2. Print-ready PDF
# -------------------------------------------------------------
g <- read.csv(file.path(csv_dir, "coaching_game_pitches.csv"), colClasses = c(game_id = "character"))
s <- read.csv(file.path(csv_dir, "coaching_summary.csv"), colClasses = c(game_id = "character"))
e <- read.csv(file.path(csv_dir, "coaching_challenges.csv"))
g$z_normalized <- (g$plate_z - g$sz_bot) / (g$sz_top - g$sz_bot)
g$event <- ifelse(g$is_challenge == 0, "Not challenged", ifelse(g$is_success == 1, "Overturned", "Upheld"))
p <- ggplot(g, aes(plate_x, z_normalized)) +
  annotate("rect", xmin = -8.5/12, xmax = 8.5/12, ymin = 0, ymax = 1, fill = NA, color = "#17313F") +
  geom_point(aes(color = event, shape = pitch_result), size = 2, alpha = .75) +
  scale_color_manual(values = c("Not challenged" = "#BAC3CC", "Overturned" = "#007F7A", "Upheld" = "#CB593D")) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom") +
  labs(x = "Horizontal location (feet; tracking coordinates)", y = "Height relative to batter zone", color = NULL, shape = "Original call")
pdf(file.path(out, "catcher_report.pdf"), width = 8.5, height = 11, family = "Helvetica", onefile = TRUE)
grid::grid.newpage()
txt <- function(label, x, y, size = 10, bold = FALSE, color = "#17313F")
  grid::grid.text(label, x = grid::unit(x, "inches"), y = grid::unit(y, "inches"), just = c("left", "top"),
                  gp = grid::gpar(fontsize = size, fontface = if (bold) "bold" else "plain", col = color))
paragraph <- function(label, y, size = 10, width = 104, color = "#17313F")
  txt(paste(strwrap(label, width = width), collapse = "\n"), .55, y, size, color = color)
txt("ABS catcher review", .55, 10.45, 25, TRUE)
txt(paste("Game", s$game_id, " | Home defense | Catcher", s$catcher_id), .55, 10.02, 11, color = "#526674")
grid::grid.rect(x = .5, y = grid::unit(9.38, "inches"), width = grid::unit(7.4, "inches"), height = grid::unit(.72, "inches"), gp = grid::gpar(fill = "#EDF5F4", col = NA))
txt(as.character(s$pitches), .75, 9.65, 23, TRUE, "#007F7A")
txt("Called pitches received", .75, 9.25, 10)
txt(sprintf("%d / %d", s$wins, s$defensive_challenges), 3.2, 9.65, 23, TRUE, "#007F7A")
txt("Defensive challenges won", 3.2, 9.25, 10)
txt(as.character(s$losses), 5.8, 9.65, 23, TRUE, "#007F7A")
txt("Defensive calls upheld", 5.8, 9.25, 10)
txt("Challenge decisions", .55, 8.78, 13, TRUE)
header <- c("Inning", "Count", "Original call", "Challenger", "Result")
xs <- c(.55, 1.35, 2.2, 4, 6.15)
for (j in seq_along(header)) txt(header[j], xs[j], 8.43, 10, TRUE)
for (i in seq_len(nrow(e))) {
  v <- c(e$inning[i], paste(e$balls[i], e$strikes[i], sep = "-"), gsub("_", " ", e$pitch_result[i]),
         ifelse(e$challenge_source[i] == "pitching_team", "Home defense", "Away offense"),
         ifelse(e$is_success[i] == 1, "Overturned", "Upheld"))
  for (j in seq_along(v)) txt(as.character(v[j]), xs[j], 8.15 - (i-1)*.24, 10)
}
txt("Pitch locations", .55, 7.43, 13, TRUE)
print(p, newpage = FALSE, vp = grid::viewport(x = .5, y = grid::unit(5.46, "inches"),
                                             width = grid::unit(7.4, "inches"), height = grid::unit(3.7, "inches")))
txt("Coaching follow-up", .55, 3.38, 13, TRUE)
paragraph("Review the challenged pitches on video: catcher sightline, glove movement and count. Compare them with the five unchallenged balls in the accompanying review list, ranked by model-estimated reversal probability. Treat these as review candidates, not confirmed missed challenges.", 3.04)
opp <- e[e$challenge_source == "hitting_team", ]
paragraph(sprintf("Against the home defense, the opposing offense challenged %d calls and overturned %d. Assess the quality of each decision alongside its game context; this single game does not establish a catcher skill level.", nrow(opp), sum(opp$is_success)), 2.27)
paragraph("Scope: called pitches in top halves only. Team labels do not distinguish a catcher challenge from a pitcher challenge. The outlined zone is a location reference, not an exact ABS boundary. Review probabilities come from a model fitted and selected without this game. Success is observed only on challenged pitches; no causal runs-saved estimate is claimed.", 1.51, size = 9, width = 117, color = "#526674")
txt("Source: supplied pitch data | Game 01103572 | Generated factual review", .55, .56, 8, color = "#526674")
dev.off()
cat("Saved", file.path(out, "catcher_report.pdf"), "\n")

# -------------------------------------------------------------
# DIAGNOSTICS
# -------------------------------------------------------------
print(coaching_summary)

