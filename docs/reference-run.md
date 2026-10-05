# Reference run

These are machine-evaluation results, not an applicant-authored process explanation.
Seed: 2027. Data checksums and package versions are recorded in the packaged run manifest.

| Target | Constant log loss | Selected log loss | Selected Brier score |
|---|---:|---:|---:|
| Any challenge | 0.121831 | 0.079349 | 0.021713 |
| Success given challenge | 0.692935 | 0.284640 | 0.089539 |
| Hitting team given challenge | 0.692645 | 0.017549 | 0.002489 |

Lower scores are better. These are separate binary tasks with different evaluation populations;
the table does not imply a combined official competition score. Challenge source is ultimately
submitted as a team label. Its holdout accuracy was 99.75% on 1,604 challenged pitches.

All three tasks selected depth-5 XGBoost on tuning games. Selected boosting rounds: 663 for
challenge, 153 for success, and 248 for source. Source prediction also has a strong simple
call-mapping baseline: tuning log loss 0.013822 versus 0.013625 for the selected model. The
small difference should not be interpreted as strong evidence that model complexity is necessary.

The 95% game-cluster bootstrap log-loss intervals on the holdout were:

* Challenge: 0.076817 to 0.081953.
* Success: 0.259527 to 0.311309.
* Source: 0.007697 to 0.031293.

Calibration is generally close in the heavily populated challenge bins. Higher-probability
challenge bins and middle-probability success bins have fewer observations and visible deviations.
No adjustment or model reselection was performed using those holdout results.

## Deliverable checks

* 34,972 test rows scored; original row order and non-target values checked before export.
* Every required probability is finite and strictly between zero and one.
* All source predictions are valid team labels.
* Freshly loaded final models reproduce the entire exported prediction table exactly.
* Unit/integration checks cover missing values, unseen categories, normalized IDs, game split
  separation, exclusion of target columns, XGBoost evaluation history and saved-model inference.
* The one-page coaching PDF was rendered and visually inspected.

The subsequent Bothra-style refactor was rerun through every stage. Training/test feature values,
all 34,972 exported prediction rows, holdout metrics and coaching counts exactly matched the
original run. The rendered coaching PDF also matched the original image. The staged source layout
and RDS locations changed; model settings and predictions did not.

## Coaching evidence

Game `01103572`, home catcher `zkrGGa8L`: 77 called pitches received. The defense challenged
two called balls, both at a 1-0 count: the third-inning call was upheld and the ninth-inning
call was overturned. No opposing hitting-team challenges were recorded against the home defense.
The team label does not identify whether the catcher or pitcher initiated either challenge.

## Limits

The held-out scores are not scores against the unlabeled test set. There are no dates for
prospective validation, the geometry features are approximations, and challenge success is
observed only for pitches that were actually challenged. The coaching review candidates require
video/context review. The personal explanation and organizational-decision essay remain for the
applicant to write using the accompanying worksheet.
