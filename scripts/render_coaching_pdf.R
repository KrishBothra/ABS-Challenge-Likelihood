# Optional print-ready copy of the factual report. Run after Model.R.
# Rscript scripts/render_coaching_pdf.R results
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "results"
g <- read.csv(file.path(out, "coaching_game_pitches.csv"), colClasses = c(game_id = "character"))
s <- read.csv(file.path(out, "coaching_summary.csv"), colClasses = c(game_id = "character"))
e <- read.csv(file.path(out, "coaching_challenges.csv"))
g$z_normalized <- (g$plate_z - g$sz_bot) / (g$sz_top - g$sz_bot)
g$event <- ifelse(g$is_challenge == 0, "Not challenged", ifelse(g$is_success == 1, "Overturned", "Upheld"))
p <- ggplot2::ggplot(g, ggplot2::aes(plate_x, z_normalized)) +
  ggplot2::annotate("rect", xmin = -8.5/12, xmax = 8.5/12, ymin = 0, ymax = 1, fill = NA, color = "#17313F") +
  ggplot2::geom_point(ggplot2::aes(color = event, shape = pitch_result), size = 2, alpha = .75) +
  ggplot2::scale_color_manual(values = c("Not challenged" = "#BAC3CC", "Overturned" = "#007F7A", "Upheld" = "#CB593D")) +
  ggplot2::theme_minimal(base_size = 10) + ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "Horizontal location (feet; tracking coordinates)", y = "Height relative to batter zone", color = NULL, shape = "Original call")
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
