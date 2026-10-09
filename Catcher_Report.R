# =============================================================
# Catcher_Report.R
#
# Game 01103572, home defense. Read the saved validation models.
# This game was excluded from fitting and model selection.
# Outputs: evidence in csv/, coaching PDF/HTML and charts in results/.
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

# -------------------------------------------------------------
# 1. Separate observed outcomes from context-based video review
# -------------------------------------------------------------
select_coaching_candidates <- function(pitches, n = 3L) {
  pitches |>
    mutate(
      late_close = inning >= 7 & abs(pre_score_home - pre_score_away) <= 2,
      two_outs = outs == 2,
      two_strike_count = strikes == 2,
      context_flags = as.integer(late_close) + as.integer(two_outs) + as.integer(two_strike_count)
    ) |>
    filter(is_challenge == 0, pitch_result == "ball",
           location_category %in% c("borderline", "obvious_strike"),
           is.finite(p_success), context_flags > 0) |>
    arrange(desc(context_flags), desc(p_success), pa_of_game, pitch_of_pa) |>
    slice_head(n = n) |>
    mutate(review_id = as.character(row_number()))
}
# Use the same coarse category as the model, not a new exact-zone classifier.
g$location_category <- features$location_category[match(g$play_id, train$play_id)]
if (is.null(g$location_category)) stop("Rebuild features and models with location_category first.")
g <- g |>
  mutate(z_normalized = (plate_z - sz_bot) / (sz_top - sz_bot))
events <- g |>
  filter(is_challenge == 1, challenge_source == "pitching_team") |>
  mutate(review_id = LETTERS[row_number()],
         result = if_else(is_success == 1, "Overturned", "Upheld"))
review <- select_coaching_candidates(g)
nc <- nrow(events)
wins <- sum(events$is_success == 1, na.rm = TRUE)
losses <- sum(events$is_success == 0, na.rm = TRUE)
rate <- if (nc) sprintf("%.0f%%", 100 * wins / nc) else "N/A"
quality <- results$success$holdout
quality_note <- sprintf("Success-model holdout log loss: %.3f (constant: %.3f). Scores are estimates, not observed outcomes.",
  quality$log_loss[quality$model != "constant"][1], quality$log_loss[quality$model == "constant"][1])
summary <- tibble(game_id = game, catcher_id = paste(unique(g$catcher_id), collapse = ", "),
  pitches_received = nrow(g), defensive_team_challenges = nc, overturned = wins,
  upheld = losses, catcher_initiated = NA_integer_, challenges_remaining = NA_integer_,
  net_run_value = NA_real_, review_candidates = nrow(review))
write_csv(g, file.path(csv_dir, "coaching_game_pitches.csv"))
write_csv(events, file.path(csv_dir, "coaching_challenges.csv"))
write_csv(review, file.path(csv_dir, "coaching_review_candidates.csv"))
write_csv(summary, file.path(csv_dir, "coaching_summary.csv"))

# -------------------------------------------------------------
# 2. Zone maps: evidence, not reconstructed Hawk-Eye rulings
# -------------------------------------------------------------
# Dimensions supplied by the user: inches, with a 2.94-inch ball diameter.
# Use raw physical height, not batter-normalized height, for true circular balls.
make_ball_polygons <- function(pitches, radius = 2.94 / 2) {
  theta <- seq(0, 2*pi, length.out = 129)
  bind_rows(lapply(seq_len(nrow(pitches)), function(i) {
    row <- pitches[rep(i, length(theta)), , drop = FALSE]
    row$ball_x <- row$plate_x * 12 + radius * cos(theta)
    row$ball_z <- row$plate_z * 12 + radius * sin(theta)
    row
  }))
}
located <- g |>
  filter(is.finite(plate_x), is.finite(plate_z)) |>
  mutate(original_call = if_else(pitch_result == "called_strike", "Called strike", "Called ball"))
marks <- bind_rows(events |> mutate(kind = result),
                   review |> mutate(kind = "Review candidate")) |>
  filter(is.finite(plate_x), is.finite(plate_z)) |>
  mutate(x_inches = plate_x * 12, z_inches = plate_z * 12,
    label_x = x_inches + if_else(plate_x < 0, -4, 4),
    label_z = z_inches + case_when(kind == "Upheld" ~ 3,
      kind == "Overturned" ~ -3, TRUE ~ 0))
balls <- make_ball_polygons(located |>
  left_join(marks |> select(play_id, kind), by = "play_id") |>
  mutate(kind = replace_na(kind, "Other")))
zone <- function() annotate("rect", xmin = -8.5, xmax = 8.5,
  ymin = 19.76, ymax = 42.61, fill = NA, color = "#163542", linewidth = .7)
base_theme <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 7), legend.key.size = grid::unit(.3, "cm"), plot.margin = margin(3, 3, 3, 3))
p <- ggplot() + zone() +
  geom_polygon(data = balls |> filter(kind == "Other"),
    aes(ball_x, ball_z, group = play_id, fill = original_call),
    color = "#7E9099", linewidth = .3, alpha = .65) +
  geom_polygon(data = balls |> filter(kind != "Other"),
    aes(ball_x, ball_z, group = play_id, fill = original_call, color = kind),
    linewidth = 1, show.legend = FALSE) +
  geom_segment(data = marks, aes(x = x_inches, y = z_inches, xend = label_x, yend = label_z), color = "#526674") +
  geom_text(data = marks, aes(label_x, label_z, label = review_id), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Called ball" = "white", "Called strike" = "#346E9A"), drop = FALSE) +
  scale_color_manual(values = c("Overturned" = "#007F7A", "Upheld" = "#C4513B", "Review candidate" = "#BC8500"), guide = "none") +
  coord_fixed(ratio = 1, xlim = c(-20, 20), ylim = c(10, 52), expand = FALSE) +
  labs(title = "Home defense: balls drawn to scale", x = "Horizontal location (in)",
       y = "Height (in)", fill = NULL) + base_theme

# Gaussian-weighted called-strike rate, not pitch density. Both halves of this
# game contribute. Mask grid points with fewer than five calls within six inches.
smooth_umpire_calls <- function(calls, grid, bandwidth = 3, min_calls = 5L) {
  values <- lapply(seq_len(nrow(grid)), function(i) {
    distance2 <- (calls$x_inches - grid$x[i])^2 + (calls$z_inches - grid$z[i])^2
    weights <- exp(-distance2 / (2 * bandwidth^2))
    nearby <- sum(distance2 <= (2 * bandwidth)^2)
    rate <- if (nearby >= min_calls && sum(weights) > 0)
      sum(weights * calls$called_strike) / sum(weights) else NA_real_
    data.frame(strike_rate = rate, nearby_calls = nearby)
  })
  cbind(grid, bind_rows(values))
}
ump <- train |>
  filter(game_id == game, umpire_id %in% g$umpire_id,
         is.finite(plate_x), is.finite(plate_z)) |>
  transmute(x_inches = plate_x * 12, z_inches = plate_z * 12,
            called_strike = as.numeric(pitch_result == "called_strike"))
contours <- smooth_umpire_calls(ump,
  expand.grid(x = seq(-20, 20, by = .5), z = seq(10, 52, by = .5)))
write_csv(contours, file.path(csv_dir, "coaching_umpire_contours.csv"))
q <- ggplot(contours, aes(x, z)) +
  geom_raster(aes(fill = strike_rate)) +
  geom_contour(aes(z = strike_rate), breaks = c(.2, .4, .6, .8),
               color = "#26796F", linewidth = .35, na.rm = TRUE) + zone() +
  scale_fill_gradient(low = "#F4F7F7", high = "#63AFA7", limits = c(0, 1),
    na.value = "white", labels = scales::label_percent(), name = "Estimated called-strike rate",
    guide = guide_colorbar(title.position = "top", barwidth = grid::unit(3, "cm"), barheight = grid::unit(.15, "cm"))) +
  coord_fixed(ratio = 1, xlim = c(-20, 20), ylim = c(10, 52), expand = FALSE) +
  labs(title = "Umpire call contours: both teams", x = "Horizontal location (in)",
       y = "Height (in)") + base_theme
if (all(is.na(contours$strike_rate))) q <- q +
  annotate("text", x = 0, y = 30, label = "Too few nearby calls", size = 3)
ggsave(file.path(out, "coaching_pitch_map.png"), p, width = 5, height = 4.5, dpi = 180)
ggsave(file.path(out, "coaching_umpire_map.png"), q, width = 5, height = 4.5, dpi = 180)

# -------------------------------------------------------------
# 3. Coach-facing text and compact decision log
# -------------------------------------------------------------
executive <- c(
  sprintf("Home defense won %d of %d challenges across %d called pitches received; %d call(s) stood. The initiator is not identified.", wins, nc, nrow(g), losses),
  sprintf("Review %d unchallenged balls flagged by count/outs/score context. These are possible review opportunities, not confirmed missed strikes.", nrow(review))
)
insight <- "Zone feel: compare the overturned and upheld calls on video. This game's small sample does not establish a repeatable blind spot."
context_label <- function(x) paste0("T", x$inning, " | ", x$balls, "-", x$strikes,
                                   " | ", x$outs, " out | ", x$pre_score_home, "-", x$pre_score_away)
log <- bind_rows(
  events |> transmute(clip = review_id, situation = context_label(events),
    decision = result, estimate = "Observed", cue = if_else(is_success == 1, "Compare sightline", "Review decision; not proven waste")),
  review |> transmute(clip = review_id, situation = context_label(review),
    decision = "No challenge", estimate = sprintf("%.0f%%", 100 * p_success),
    cue = sub("; $", "", paste0(if_else(late_close, "Late/close; ", ""),
                 if_else(two_outs, "2 outs; ", ""), if_else(two_strike_count, "2 strikes", ""))))
)
write_csv(log, file.path(csv_dir, "coaching_decision_log.csv"))
takeaways <- c(
  "Zone judgment: review A/B and numbered clips at plate crossing. Compare ball edge and supplied zone; do not infer pitch-type blind spots without pitch classifications and more games.",
  "Game management: discuss the count, outs and score before each decision. Check the team's challenge log before judging whether an early loss constrained late-game options.",
  "Receiving mechanics: use video to assess sightline and glove movement. Tracking rows cannot establish framing interference or its effect on the umpire."
)

# -------------------------------------------------------------
# 4. One-page PDF (letter) and matching HTML
# -------------------------------------------------------------
pdf(file.path(out, "catcher_report.pdf"), width = 8.5, height = 11, family = "Helvetica")
grid::grid.newpage()
text_at <- function(label, x, y, size = 9, bold = FALSE, color = "#163542") {
  grid::grid.text(label, x = grid::unit(x, "inches"), y = grid::unit(y, "inches"),
    just = c("left", "top"), gp = grid::gpar(fontsize = size,
      fontface = if (bold) "bold" else "plain", col = color, lineheight = 1.12))
}
para <- function(label, y, width = 117, size = 8.5, x = .45, color = "#526674") {
  text_at(paste(strwrap(label, width = width), collapse = "\n"), x, y, size, color = color)
}
heading <- function(label, y) text_at(label, .45, y, 11, TRUE)
text_at("CATCHER | POSTGAME ABS REVIEW", .45, 10.65, 21, TRUE)
text_at(sprintf("Game %s  |  Home defense  |  Catcher %s", game, summary$catcher_id), .45, 10.23, 9)
heading("1  EXECUTIVE SUMMARY", 9.96)
para(paste0("- ", executive[1]), 9.71)
para(paste0("- ", executive[2]), 9.39)
para(insight, 9.07, size = 8.5)
heading("2  CHALLENGE SUMMARY", 8.69)
xs <- c(.45, 2.40, 4.35, 6.28)
values <- c(sprintf("%d team / N/A catcher", nc), rate, "Not available", "Not available")
labels <- c("Challenges initiated", "Team overturn rate", "Challenges remaining", "Net run value / WPA")
for (j in 1:4) {
  text_at(values[j], xs[j], 8.42, 12, TRUE, "#007F7A")
  text_at(labels[j], xs[j], 8.18, 8)
}
para("Team source cannot identify catcher vs pitcher. Allotment rules/history, baserunners and run/win-expectancy inputs are not supplied.", 7.98, size = 7.7, width = 130)
heading("3  STRIKE-ZONE REVIEW", 7.66)
print(p, newpage = FALSE, vp = grid::viewport(x = .265, y = grid::unit(6.06, "inches"),
  width = grid::unit(3.95, "inches"), height = grid::unit(2.85, "inches")))
print(q, newpage = FALSE, vp = grid::viewport(x = .745, y = grid::unit(6.06, "inches"),
  width = grid::unit(3.95, "inches"), height = grid::unit(2.85, "inches")))
para("2.94-in balls; solid zone: 17 in wide, 19.76-42.61 in high. Green/red/gold rims: overturned/upheld/review. Contours estimate called-strike rate (20/40/60/80%), using 3-in smoothing; blank means fewer than 5 calls within 6 in. Reference zone, not verified ABS.", 4.59, size = 7.5, width = 139)
heading("4  HIGH-LEVERAGE REVIEW & POSSIBLE MISSED OPPORTUNITIES", 4.13)
text_at("Clip", .45, 3.87, 8, TRUE)
text_at("Inning | count | outs | score H-A", .9, 3.87, 8, TRUE)
text_at("Decision", 3.42, 3.87, 8, TRUE)
text_at("Overturn*", 4.64, 3.87, 8, TRUE)
text_at("Review cue", 5.62, 3.87, 8, TRUE)
# The report is intentionally scoped to one game; keep the one-page log bounded.
if (nrow(log) > 5) stop("More than five decision rows: adjust one-page layout before export.")
for (i in seq_len(nrow(log))) {
  y <- 3.65 - (i-1)*.21
  text_at(log$clip[i], .45, y, 8.2, TRUE)
  text_at(log$situation[i], .9, y, 8.2)
  text_at(log$decision[i], 3.42, y, 8.2)
  text_at(log$estimate[i], 4.64, y, 8.2)
  text_at(log$cue[i], 5.62, y, 7.7)
}
para("*Estimated reversal IF challenged, not advice to challenge. Candidates exclude obvious balls/unknown locations, then rank late-close (7th+, within 2 runs), two-out and two-strike flags; model score breaks ties. No baserunner or leverage index is available. Upheld does not prove a wasted challenge.", 2.54, size = 7.5, width = 137)
heading("5  COACHING TAKEAWAYS & ADJUSTMENTS", 1.97)
for (i in seq_along(takeaways)) para(paste0("- ", takeaways[i]), 1.72 - (i-1)*.36, size = 8, width = 125)
para(paste("Source: supplied called-pitch data. This game was excluded from model training/tuning.", quality_note, "Catcher-specific intent is unknown."), .48, size = 7, width = 150)
dev.off()

html_rows <- paste(vapply(seq_len(nrow(log)), function(i) paste0("<tr>",
  paste0("<td>", unlist(log[i, ]), "</td>", collapse = ""), "</tr>"), character(1)), collapse = "\n")
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><title>Catcher ABS review</title>',
 '<style>body{font:14px Arial;max-width:950px;margin:30px auto;color:#163542}h2{font-size:18px}td,th{padding:7px;text-align:left}img{width:48%}.note{font-size:12px}</style>',
 '<h1>Catcher | Postgame ABS review</h1><p>Game ', game, ' | Home defense | Catcher ', summary$catcher_id, '</p>',
 '<h2>1. Executive summary</h2><ul><li>', paste(executive, collapse = '</li><li>'), '</li></ul><p>', insight, '</p>',
 '<h2>2. Challenge summary</h2><p>', nc, ' defensive team challenges; catcher initiator unknown. ', rate,
 ' team overturn rate. Challenges remaining and net run value/WPA: unavailable.</p>',
 '<h2>3. Strike-zone review</h2><img src="coaching_pitch_map.png" alt="Challenge outcomes and review candidates">',
 '<img src="coaching_umpire_map.png" alt="Observed umpire call rates in populated cells">',
 '<p class="note">Balls are 2.94 inches in diameter with equal axis scale. White = called ball; blue = called strike. Green/red/gold rims = overturned/upheld/review. One solid zone is 17 inches wide and 19.76-42.61 inches high, with no expanded or dashed outline. Umpire contours estimate called-strike rate with 3-inch Gaussian smoothing; contour levels are 20/40/60/80%. Blank regions have fewer than five calls within six inches. This is a reference zone, not verified ABS.</p>',
 '<h2>4. High-leverage review &amp; possible missed opportunities</h2><table><tr><th>Clip</th><th>Situation</th><th>Decision</th><th>Overturn estimate</th><th>Cue</th></tr>', html_rows, '</table>',
 '<p class="note">Unchallenged outcomes are unknown. Candidates exclude obvious balls/unknown locations and rank context flags before model score. No baserunners or leverage index. Upheld does not mean wasted. Probabilities estimate reversal if challenged; they are not challenge recommendations.</p>',
 '<h2>5. Coaching takeaways &amp; adjustments</h2><ul><li>', paste(takeaways, collapse = '</li><li>'), '</li></ul>',
 '<p class="note">Model excluded this game. ', quality_note, ' Team labels cannot identify the initiator. Allotment, run value and framing effects cannot be established from the supplied data.</p></html>')
writeLines(html, file.path(out, "catcher_report.html"))
cat("Saved coach-facing PDF/HTML in", out, "and supporting CSVs in", csv_dir, "\n")
print(summary)
