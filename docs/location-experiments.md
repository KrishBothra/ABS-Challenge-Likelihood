# Location feature experiments

All runs used the same game split and tuning procedure. Lower log loss is better.

| Feature set | Challenge | Overturn if challenged | Challenging team |
|---|---:|---:|---:|
| Full original features | 0.07934949 | 0.28463972 | 0.01754896 |
| Remove call_disagreement only | 0.07958150 | 0.30499250 | 0.01780311 |
| Coarse category, 2.9-inch center clearance | 0.09140396 | 0.60787812 | 0.01727935 |
| No explicit location features | 0.11456091 | 0.67611079 | 0.01739478 |

The no-location model retained 25 features; all three selected depth-3 XGBoost.
Its challenge split-gain leaders were terminal_call (36.1%), called_strike (24.5%),
and strikes (11.4%). Movement and context may still correlate with location.

User decision: retain this experiment for comparison, return to a coarse location
category, and increase the obvious-pitch clearance. Selected revised geometry: 4-inch clearance beyond the entire ball, using a
1.5-inch radius (5.5-inch ball-center clearance; 28-inch outer width). The previous category used ball-center clearance of 2.9 inches from
all supplied zone edges, with obvious strike, obvious ball, borderline, and unknown
categories. No raw location or margin features were supplied to those models.

Repeated inspection of holdout results makes these exploratory comparisons; the
holdout is no longer an untouched final model-selection test.

Earlier experiments were preserved as separate downloadable bundles.

## Four-inch whole-ball clearance rerun

- challenge: log loss 0.0999520399905274 (xgb_depth5).
- success: log loss 0.676809340508867 (xgb_depth3).
- source: log loss 0.01748712074337 (xgb_depth3).

Boundary checks passed; 26 features; 34,972 regenerated test predictions.
