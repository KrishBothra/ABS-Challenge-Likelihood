clip_prob <- function(p) pmin(1 - 1e-6, pmax(1e-6, as.numeric(p)))
log_loss <- function(y, p) { p <- clip_prob(p); -mean(y * log(p) + (1-y) * log1p(-p)) }
metrics <- function(y, p) {
  p <- clip_prob(p)
  c(log_loss = log_loss(y, p), brier = mean((p-y)^2), observed_rate = mean(y), predicted_rate = mean(p))
}

predict_candidate <- function(model, x) {
  if (model$type == "constant") return(rep(model$p, nrow(x)))
  if (model$type == "call_mapping") return(ifelse(as.numeric(x[, "called_strike"]) == 1, model$strike_p, model$ball_p))
  if (model$type == "ridge") return(clip_prob(predict(model$fit, newx = x, s = model$lambda, type = "response")))
  clip_prob(predict(model$fit, xgboost::xgb.DMatrix(x, nthread = 2)))
}

fit_task <- function(features, y, split, task, out, seed = 2027, max_rounds = 700L) {
  valid <- !is.na(y)
  a <- which(split == "fit" & valid)
  b <- which(split == "tune" & valid)
  h <- which(split == "holdout" & valid)
  stopifnot(length(unique(y[a])) == 2, length(unique(y[b])) == 2, length(unique(y[h])) == 2)
  encoder <- fit_encoder(features[a, , drop = FALSE])
  x <- encode_features(features, encoder)
  candidates <- list(constant = list(type = "constant", p = mean(y[a])))
  if (task == "source") {
    z <- features$called_strike[a]
    candidates$call_mapping <- list(type = "call_mapping",
      strike_p = (sum(y[a][z == 1]) + 1) / (sum(z == 1) + 2),
      ball_p = (sum(y[a][z == 0]) + 1) / (sum(z == 0) + 2))
  }
  lambdas <- 10^seq(-1, -5, length.out = 20)
  ridge <- glmnet::glmnet(x[a, ], y[a], family = "binomial", alpha = 0, lambda = lambdas, standardize = TRUE)
  rp <- predict(ridge, newx = x[b, ], type = "response")
  best_lambda <- lambdas[which.min(apply(rp, 2, function(p) log_loss(y[b], p)))]
  candidates$ridge <- list(type = "ridge", fit = ridge, lambda = best_lambda)
  tuning <- list(data.frame(model = "ridge", parameter = lambdas,
                           log_loss = apply(rp, 2, function(p) log_loss(y[b], p))))
  dtrain <- xgboost::xgb.DMatrix(x[a, ], label = y[a], nthread = 2)
  dtune <- xgboost::xgb.DMatrix(x[b, ], label = y[b], nthread = 2)
  for (depth in c(3L, 5L)) {
    message(task, ": fitting boosted trees, depth ", depth)
    params <- list(objective = "binary:logistic", eval_metric = "logloss", tree_method = "hist",
                   max_depth = depth, eta = .04, min_child_weight = 8, subsample = .85,
                   colsample_bytree = .85, lambda = 5, nthread = 2, seed = seed)
    trial <- xgboost::xgb.train(params, dtrain, nrounds = max_rounds, evals = list(tune = dtune),
                              early_stopping_rounds = 50, verbose = 0)
    history <- as.data.frame(attr(trial, "evaluation_log"))
    stopifnot(nrow(history) > 0, "tune_logloss" %in% names(history))
    rounds <- which.min(history$tune_logloss)
    # Refit the exact selected round count instead of relying on version-specific prediction defaults.
    fit <- xgboost::xgb.train(params, dtrain, nrounds = rounds, verbose = 0)
    candidates[[paste0("xgb_depth", depth)]] <- list(type = "xgb", fit = fit, params = params, rounds = rounds)
    history$model <- paste0("xgb_depth", depth)
    write.csv(history, file.path(out, paste0(task, "_depth", depth, "_learning_curve.csv")), row.names = FALSE)
  }
  tune_metrics <- do.call(rbind, lapply(names(candidates), function(nm) {
    data.frame(task = task, model = nm, t(metrics(y[b], predict_candidate(candidates[[nm]], x[b, ]))))
  }))
  selected_name <- tune_metrics$model[which.min(tune_metrics$log_loss)]
  selected <- candidates[[selected_name]]
  write.csv(tune_metrics, file.path(out, paste0(task, "_tuning.csv")), row.names = FALSE)
  write.csv(tuning[[1]], file.path(out, paste0(task, "_ridge_tuning.csv")), row.names = FALSE)
  # Holdout is scored only after the model choice is frozen on tune games.
  holdout <- do.call(rbind, lapply(c("constant", selected_name), function(nm) {
    data.frame(task = task, model = nm, t(metrics(y[h], predict_candidate(candidates[[nm]], x[h, ]))))
  }))
  holdout <- unique(holdout)
  hp <- predict_candidate(selected, x[h, ])
  list(selected = selected, selected_name = selected_name, encoder = encoder,
       holdout = holdout, holdout_indices = h, holdout_predictions = hp,
       tune = tune_metrics, validation_model = selected)
}

refit_task <- function(features, y, selected) {
  keep <- which(!is.na(y))
  encoder <- fit_encoder(features[keep, , drop = FALSE])
  x <- encode_features(features[keep, , drop = FALSE], encoder)
  model <- selected
  if (model$type == "constant") model$p <- mean(y[keep])
  if (model$type == "call_mapping") {
    z <- features$called_strike[keep]
    model$strike_p <- (sum(y[keep][z == 1]) + 1) / (sum(z == 1) + 2)
    model$ball_p <- (sum(y[keep][z == 0]) + 1) / (sum(z == 0) + 2)
  }
  if (model$type == "ridge") model$fit <- glmnet::glmnet(x, y[keep], family = "binomial", alpha = 0,
                                                         lambda = model$lambda, standardize = TRUE)
  if (model$type == "xgb") model$fit <- xgboost::xgb.train(model$params,
    xgboost::xgb.DMatrix(x, label = y[keep], nthread = 2), nrounds = model$rounds, verbose = 0)
  list(model = model, encoder = encoder)
}

calibration_table <- function(y, p, task) {
  # Fixed probability bins avoid arbitrary splitting of equal predictions.
  breaks <- if (task == "challenge") c(0, .005, .01, .02, .05, .1, .2, .4, .6, .8, 1) else seq(0, 1, .1)
  bin <- cut(p, breaks = breaks, include.lowest = TRUE)
  do.call(rbind, lapply(levels(bin), function(z) {
    k <- which(bin == z)
    if (!length(k)) return(NULL)
    data.frame(task = task, bin = z, n = length(k), predicted = mean(p[k]), observed = mean(y[k]))
  }))
}

cluster_bootstrap <- function(y, p, baseline, games, seed = 2027, reps = 500) {
  set.seed(seed)
  p <- clip_prob(p); baseline <- clip_prob(baseline)
  l <- -(y * log(p) + (1-y) * log1p(-p))
  b <- -(y * log(baseline) + (1-y) * log1p(-baseline))
  totals <- aggregate(cbind(loss = l, baseline_loss = b, n = rep(1, length(y))), list(game = games), sum)
  draws <- replicate(reps, {
    ix <- sample.int(nrow(totals), nrow(totals), replace = TRUE)
    c(log_loss = sum(totals$loss[ix]) / sum(totals$n[ix]),
      improvement = sum(totals$baseline_loss[ix] - totals$loss[ix]) / sum(totals$n[ix]))
  })
  data.frame(metric = rownames(draws), lower = apply(draws, 1, quantile, .025),
             upper = apply(draws, 1, quantile, .975))
}
