# ABS Challenge Likelihood

Reproducible R models and diagnostics for the Mariners 2027 Data Science Intern Problem Set.
The project predicts whether a called pitch is challenged, which team challenges it, and whether
the challenge succeeds. It also produces a factual coaching review for the home defense in game
`01103572`.

## Run

Open `Challenge_Likelihood.Rproj`, or use a terminal from this directory:

```sh
Rscript Setup.R
Rscript tests/run_tests.R
Rscript Run_All.R Data results
```

The layout follows the stage-based scripts in
[`Teamworks_CMSAC_2026/Bothra`](https://github.com/KrishBothra/Teamworks_CMSAC_2026/tree/main/Bothra):
libraries at the top, native `|>` pipelines, named sections, explicit model blocks, RDS handoffs,
and console diagnostics at the end. Each stage can also be run separately from the project root:

| Run order | Script | Main saved output |
|---|---|---|
| 1 | `Data_Wrangling.R` | `Data/datasets/pitch_split.rds` |
| 2 | `features.R` | `Data/datasets/pitch_split_feat.rds` |
| 3 | `tune.R` | `Data/datasets/model_*_validation.rds` |
| 4 | `Model.R` | `Data/datasets/model_challenge.rds`, `model_success.rds`, `model_source.rds` |
| 5 | `Predict.R` | `csv/data-test-predictions.csv` |
| 6 | `Diagnostics.R` | Holdout tables, calibration and feature importance |
| 7 | `Variable_Importance.R` | All-feature VIP chart in `results/` |
| Optional | `Catcher_Report.R` | Coaching evidence in `csv/`, HTML and PDF in `results/` |

For example, `Rscript features.R Data results` rebuilds just the feature tables after wrangling.
In RStudio, open the project and source these scripts in order. All stages accept the same optional
data/output directory arguments; intermediates are stored under the selected data directory's
`datasets` folder. `functions.R` contains shared input, encoding and probability calculations;
it has no data-reading or training side effects. `features.R`, `tune.R` and `Model.R` own their
respective feature, tuning and fitting code.

The existing native XGBoost fitting API is retained so the organization/style change does not
change model behavior. No tidymodels recipe conversion or new model search is introduced.

R 4.5.2 and XGBoost 3.2.0.1 were used for development. The code uses the XGBoost 3.x `evals` API.
Exact package versions, input MD5 checksums, selected models and random seed are written to
`results/session-info.txt` and `results/run_manifest.json`. To restore the reference package
versions, use `renv::restore()` with the included `renv.lock`.

Inputs may be CSV or XLSX (`data-train` and `data-test`). CSV takes precedence if both exist.
The source data is never overwritten. Parsed data is cached by input checksum in `.cache/data`.

The main output is `csv/data-test-predictions.csv`: all original test columns and rows, with
the three requested prediction columns populated. IDs are normalized to strings; game IDs are
zero-padded to eight digits. Numeric formatting may differ from Excel, but values are preserved.

To score again without retraining:

```sh
Rscript Predict.R Data results
```

This reads the saved feature table and final models from `Data/datasets`; it does not rerun tuning.
If input files change, rebuild the preceding stages to keep the saved tables and models consistent.

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

The 26 features include speed, movement, extension, original call and game context.
The only explicit pitch-location input is `location_category`: obvious strike,
obvious ball, borderline or unknown. An obvious pitch requires 4 inches of clearance
beyond the entire ball, using a 1.5-inch radius: 5.5 inches of center clearance.
Obvious balls are beyond at least one supplied zone edge by that amount (horizontal
cutoffs +/-14 inches, or 28-inch outer width). Obvious strikes clear every edge inward
by that amount (horizontal cutoffs +/-3 inches, or 6-inch inner width). The same rule
applies to supplied top and bottom bounds. This conservative reference geometry is
not an exact ABS ruling. Missing/invalid locations are unknown. The threshold is fixed.

Coordinates and zone bounds are used only to form this category. All raw location,
zone dimension, margin, normalized-location, call_disagreement and release-coordinate/
angle predictors are excluded. Catcher, umpire, venue, handedness and count remain.
Pitcher/batter IDs, outcome fields and game/play IDs are excluded. Numeric missing
values use fitting-set medians with missing indicators; unseen categories have a fallback.

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
* `catcher_report.html` and `catcher_report.pdf`: one-page coaching review with `coaching_pitch_map.png`.
* `coaching_review_candidates.csv`: five unchallenged balls for video review, not confirmed errors.
* `Data/datasets/`: intermediate tables, final models/encoders and validation models (outside `results`).

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


## Clean output folders

- `csv/`: every CSV export, including predictions, metrics, tuning, importance and coaching evidence.
- `results/`: catcher reports, model charts, run manifest and session information.
- `Data/datasets/`: saved feature tables, fitted models and encoders.

Run `Rscript Run_All.R Data results csv` to choose all three directories explicitly.
If the third argument is omitted, the CSV folder is a sibling of the results folder.
Each stage safely migrates CSVs directly inside an existing results folder; it stops
rather than overwriting a different destination file. Other files are left intact.
The catcher report remains optional; run `Rscript Catcher_Report.R Data results csv`.
The VIP is now included in Run_All.R. Rerun from features.R through tuning/refitting
when switching feature versions; do not reuse old fitted models with new features.

Latest holdout log loss: challenge 0.09995204, success 0.67680934, source 0.01748712.
See `docs/location-experiments.md` for prior experiments. Those comparisons reuse
the same holdout and are exploratory, not an untouched final test.

## Coach-facing postgame report

Run `Rscript Catcher_Report.R Data results csv` after tuning and building features.
The report remains optional in `Run_All.R`. It produces a one-page letter PDF and
matching HTML with five sections: executive summary, challenge metrics, two zone maps,
context-prioritized decision review, and coaching adjustments. Figures and reports
stay in `results/`; evidence, decision logs and umpire-bin tables stay in `csv/`.

Defensive team challenges are distinguished from catcher-specific attribution (not
available). Remaining challenges and run value/WPA are marked unavailable because
allotment rules/history and baserunner/expectancy inputs are not supplied. Pitch type,
framing interference, exact ABS geometry and midpoint tracking are not established.

Numbered review candidates are unchallenged balls in the model's borderline or
obvious-strike category. Candidates must have at least one context flag: 7th inning
or later within two runs, two outs, or two strikes. They are ordered by number of
flags, then estimated reversal probability. This is a video queue, not a leverage
index or proof of a missed strike. Actual challenges are labeled A/B; upheld calls
are not automatically described as wasted. All pitch IDs remain in the CSV evidence.

The umpire map shows a Gaussian-smoothed called-strike rate from both halves of
the game, with a 3-inch bandwidth and contours at 20%, 40%, 60% and 80%. Regions
with fewer than five calls within six inches are blank. This is a descriptive
estimate from one game, not pitch density or a verified umpire boundary. Grid
estimates and local counts are exported to `csv/coaching_umpire_contours.csv`.

### To-scale pitch graphics

A single solid reference zone is 17 inches wide, from 19.76 to 42.61 inches high.
There is no radius-expanded outline or dashed inner rectangle. Every ball is a
2.94-inch-diameter polygon with equal physical axis scales. White circles are called
balls, blue circles are called strikes; green/red/gold rims identify overturns,
upheld calls and review candidates. Model features and predictions are unchanged.
