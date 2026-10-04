save_diagnostics <- function(train, results, out, seed) {
  all_metrics <- do.call(rbind, lapply(results, `[[`, "holdout"))
  write.csv(all_metrics, file.path(out, "holdout_metrics.csv"), row.names = FALSE)
  all_cal <- list(); all_pred <- list(); ci <- list()
  for (task in names(results)) {
    r <- results[[task]]; ix <- r$holdout_indices
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
  p <- ggplot2::ggplot(cal, ggplot2::aes(predicted, observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey60") +
    ggplot2::geom_point(ggplot2::aes(size = n), color = "#007F7A") +
    ggplot2::facet_wrap(~task, scales = "free") + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = "Probability calibration on untouched holdout games",
                  x = "Mean predicted probability", y = "Observed frequency", size = "Pitches")
  ggplot2::ggsave(file.path(out, "calibration.png"), p, width = 11, height = 4, dpi = 160)
  for (task in names(results)) {
    r <- results[[task]]
    if (r$selected$type == "xgb") {
      importance <- as.data.frame(xgboost::xgb.importance(model = r$selected$fit))
      write.csv(importance, file.path(out, paste0(task, "_importance.csv")), row.names = FALSE)
    }
  }
  invisible(all_metrics)
}

write_coaching_report <- function(train, features, results, out, game = "01103572") {
  ix <- which(train$game_id == game & train$is_top_inning)
  if (!length(ix)) stop("Coaching game/home defense not found: ", game)
  g <- train[ix, , drop = FALSE]
  for (task in c("challenge", "success")) {
    r <- results[[task]]
    g[[paste0("p_", task)]] <- predict_candidate(r$validation_model,
      encode_features(features[ix, , drop = FALSE], r$encoder))
  }
  g <- g[order(g$pa_of_game, g$pitch_of_pa), ]
  own <- g$is_challenge == 1 & g$challenge_source == "pitching_team"
  opp <- g$is_challenge == 1 & g$challenge_source == "hitting_team"
  nc <- sum(own); wins <- sum(g$is_success[own]); losses <- nc - wins
  write.csv(g, file.path(out, "coaching_game_pitches.csv"), row.names = FALSE)
  events <- g[g$is_challenge == 1, c("play_id", "inning", "balls", "strikes", "outs", "pitch_result",
                                    "challenge_source", "is_success", "p_success", "plate_x", "plate_z")]
  write.csv(events, file.path(out, "coaching_challenges.csv"), row.names = FALSE)
  # Rank review candidates, without pretending unobserved outcomes are known mistakes.
  review <- g[g$is_challenge == 0 & g$pitch_result == "ball", ]
  review <- head(review[order(-review$p_success), ], 5)
  write.csv(review, file.path(out, "coaching_review_candidates.csv"), row.names = FALSE)
  g$z_normalized <- (g$plate_z - g$sz_bot) / (g$sz_top - g$sz_bot)
  g$event <- ifelse(g$is_challenge == 0, "Not challenged",
                    ifelse(g$is_success == 1, "Overturned", "Upheld"))
  p <- ggplot2::ggplot(g, ggplot2::aes(plate_x, z_normalized)) +
    ggplot2::annotate("rect", xmin = -8.5/12, xmax = 8.5/12, ymin = 0, ymax = 1,
                      fill = NA, color = "#1C3545", linewidth = .6) +
    ggplot2::geom_point(ggplot2::aes(color = event, shape = pitch_result), alpha = .65, size = 2.2) +
    ggplot2::scale_color_manual(values = c("Not challenged" = "#BAC3CC", "Overturned" = "#007F7A", "Upheld" = "#CB593D")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(x = "Horizontal location (feet; tracking coordinates)", y = "Height relative to batter zone",
                  color = NULL, shape = "Original call") +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(out, "coaching_pitch_map.png"), p, width = 7, height = 3.7, dpi = 160)
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
  invisible(data.frame(game_id = game, catcher_id = paste(unique(g$catcher_id), collapse = ","),
                       pitches = nrow(g), defensive_challenges = nc, wins = wins, losses = losses))
}
