# Load only the pure ranking function; do not render during unit tests.
for (expr in parse("Catcher_Report.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("select_coaching_candidates"))) eval(expr)
}
d <- data.frame(play_id = letters[1:7], inning = c(9, 1, 9, 9, 9, 9, 1),
  pre_score_home = 4, pre_score_away = 4, outs = c(2, 2, 2, 2, 2, 2, 0),
  strikes = c(2, 0, 2, 2, 2, 2, 0), balls = 0,
  is_challenge = c(0, 0, 0, 0, 1, 0, 0),
  pitch_result = c(rep("ball", 5), "called_strike", "ball"),
  location_category = c("borderline", "borderline", "obvious_ball", "unknown",
                        "borderline", "borderline", "borderline"),
  p_success = c(.2, .9, .99, .99, .99, .99, .99),
  pa_of_game = 1:7, pitch_of_pa = 1)
r <- select_coaching_candidates(d)
stopifnot(identical(r$play_id, c("a", "b")), identical(r$review_id, c("1", "2")))
stopifnot(nrow(select_coaching_candidates(d[0, ])) == 0)
stopifnot(nrow(select_coaching_candidates(d, n = 1L)) == 1)
cat("PASS: coaching candidates exclude challenged/strike/obvious-ball/unknown pitches and rank context before model score.\n")

for (expr in parse("Catcher_Report.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("make_ball_polygons"))) eval(expr)
}
b <- make_ball_polygons(data.frame(play_id = c("a", "b"), plate_x = c(0, 1), plate_z = c(2, 3)))
for (id in c("a", "b")) {
  z <- b[b$play_id == id, ]
  stopifnot(abs(diff(range(z$ball_x)) - 2.94) < 1e-10,
            abs(diff(range(z$ball_z)) - 2.94) < 1e-10,
            max(abs(sqrt((z$ball_x-z$plate_x*12)^2 + (z$ball_z-z$plate_z*12)^2)-1.47)) < 1e-10)
}
cat("PASS: pitch polygons have 2.94-inch diameters and 1.47-inch radius in physical coordinates.\n")
