# ABS Challenge Likelihood

Reproducible R models and diagnostics for the Mariners 2027 Data Science Intern Problem Set.
The project predicts whether a called pitch is challenged, which team challenges it, and whether
the challenge succeeds. It also produces a factual coaching review for the home defense in game
`01103572`.

## Run

Open `Challenge_Likelihood.Rproj`, or use a terminal from this directory:

```sh
Rscript scripts/install_dependencies.R
Rscript tests/run_tests.R
Rscript Model.R Data results
Rscript scripts/render_coaching_pdf.R results
```

R 4.5.2 and XGBoost 3.2.0.1 were used for development. The code uses the XGBoost 3.x `evals` API.
Exact package versions, input MD5 checksums, selected models and random seed are written to
`results/session-info.txt` and `results/run_manifest.json`. To restore the reference package
versions, use `renv::restore()` with the included `renv.lock`.

Inputs may be CSV or XLSX (`data-train` and `data-test`). CSV takes precedence if both exist.
The source data is never overwritten. Parsed data is cached by input checksum in `.cache/data`.

The main output is `results/data-test-predictions.csv`: all original test columns and rows, with
the three requested prediction columns populated. IDs are normalized to strings; game IDs are
zero-padded to eight digits. Numeric formatting may differ from Excel, but values are preserved.

To score again without retraining:

```sh
Rscript scripts/predict.R Data/data-test.xlsx results/models predictions.csv
```

## Data and targets

The supplied workbooks contain 309,063 labeled called pitches across 2,008 games and 34,972
test pitches across 223 disjoint games. The training set has 8,329 challenges (2.69%), of which
4,097 were successful (49.19%). Missing outcome cells are not negative labels.

* `p_challenge`: probability of any challenge, trained on all labeled pitches.
* `challenge_source`: predicted team conditional on a challenge. The source model predicts
  the probability of `hitting_team`; a 0.5 cutoff supplies the required label. Additional
  probabilities are written to `source_probabilities.csv`.
* `p_success_g_challenge`: probability of reversal conditional on a challenge, trained only
  on actually challenged pitches. This does not establish what would have happened on every
  unchallenged pitch.

The observed success column is `is_success`, although the PDF says `is_successful`.
Twenty-nine source labels conflict with the usual original-call mapping. They are retained and
exported to `challenge_source_exceptions.csv`; the workflow does not silently relabel them.

## Modeling design

With seed 2027, games are assigned to 60% fitting, 20% tuning, and 20% untouched holdout.
Game `01103572` is reserved separately for coaching analysis. There is no date field, so the
split measures generalization across games, not future-season performance.

For each target, the candidates are an empirical constant, ridge logistic regression and
XGBoost trees at depths 3 and 5. The source task also tests a smoothed call-to-team mapping.
Ridge penalty and boosting iterations are selected on tuning games by log loss. Tree training
is capped at 700 rounds with 50-round early stopping. The winning candidate is chosen before
looking at the holdout. Holdout scores therefore describe a model fitted on the fitting games;
the final submission models are subsequently refitted on all available labeled rows.

Features include pitch measurements, count, inning, score difference, batting side, and distances
to strike-zone boundaries. The nominal 17-inch plate and 1.45-inch baseball-radius expansion are
geometry proxies; raw coordinates and the supplied batter bounds remain available to the model.
This is not an exact reconstruction of official ABS geometry, including its measurement plane
and treatment of corners. Catcher, umpire, venue, handedness and count are one-hot encoded.
Pitcher/batter IDs are omitted in this first version. Missing numerical inputs use fitting-set
medians and missing indicators. Unseen categories have an explicit fallback. No outcome,
game ID, play ID or future-pitch information is a model feature.

There is no oversampling or class weighting: the objective is calibrated probabilities on the
natural event distribution. Evaluation includes log loss, Brier score, calibration bins and
500 game-cluster bootstrap confidence intervals. Calibration is diagnosed, not post-hoc tuned
on the holdout. Bootstrap intervals describe uncertainty across these holdout games, not all
possible modeling or future-season uncertainty.

Remaining challenges are not used as a feature. They are absent from the test data, and rebuilding
them from true test outcomes would leak labels. Player history features, stronger geometric
modeling and properly nested calibration are future experiments rather than assumed improvements.

## Outputs

* `data-test-predictions.csv`: submission data, in original test-row order.
* `holdout_metrics.csv`: constant baseline and selected-model scores, separately for each target.
* `*_tuning.csv`, `*_learning_curve.csv`: selection evidence.
* `split_manifest.csv`: exact game-based split for every training pitch.
* `calibration.png` and `calibration.csv`: held-out probability checks.
* `holdout_bootstrap_intervals.csv`: clustered uncertainty intervals.
* `coaching_summary.csv`, `coaching_challenges.csv`, `coaching_game_pitches.csv`:
  factual evidence for the target game.
* `catcher_report.html`: printable one-page coaching review with `coaching_pitch_map.png`.
* `coaching_review_candidates.csv`: five unchallenged balls for video review, not confirmed errors.
* `models/`: final models/encoders and separately saved validation models.

The home catcher is identified from top halves of innings. A pitching-team challenge can be made
by the catcher or pitcher; the data does not identify the initiator. The report never assigns all
defensive challenges to the catcher, infers exact run value, or treats unobserved reversals as facts.

## Assignment writing

The assignment permits AI coding tools but explicitly requires the process explanation to be
written in the applicant's own words. `docs/writing-guide.md` provides questions and evidence
locations for that explanation and the separate 300-word organizational decision response.
It is a worksheet, not a completed personal submission. The repository README documents software
behavior and should not be submitted as the applicant's own process explanation.

## Sources

The assignment PDF and data dictionary are provided in this repository. The assignment links to
[MLB's ABS rules overview](https://www.mlb.com/news/abs-challenge-system-mlb-2026).
The data covers Minor League games; MLB 2026 rules should not be assumed to identify the historical
challenge allotment or strike-zone geometry for this dataset without confirmation.
