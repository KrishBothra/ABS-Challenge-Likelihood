# Personal writing worksheet

## Process explanation for R&D

Write this explanation yourself, as the assignment requires. Use the executed results as evidence,
and make sure you can explain the modeling choices before submitting.

1. What are the three targets? Why is a non-challenge not a failed challenge?
2. What did the data audit reveal? Explain missing tracking measurements and the 29 unusual source labels.
3. Why split by game? What can this holdout say, and what can it not say without dates?
4. Which model won for each target? Consult `results/*_tuning.csv` and `run_manifest.json`.
5. How much did log loss improve over the constant baseline? Use `holdout_metrics.csv`.
6. What does the calibration plot reveal, including sparse high-probability bins?
7. Why might location relative to the zone and the original call predict challenge behavior?
8. What assumptions remain in the geometry proxy and the unchallenged-pitch review list?
9. What would you test next with more time: exact ABS geometry, challenge availability, player histories,
   time-based validation, or better calibrated probabilities? Explain how you would test the change.

## Coaching review

Review the generated one-page report alongside the pitch-level CSV and, if available, video.
Check that team-level events are not attributed uniquely to the catcher. The five suggested review
pitches are ranked model estimates, not proven missed reversals. The model used here never fitted
on this game, and the game never influenced model selection.

## Approximately 300-word Mariners decision response

Choose a decision you personally consider a mistake. This needs your judgment and supporting
sources; the pipeline cannot infer your position from the pitch data.

* Identify the decision and its date.
* Gather sources published on or before that date: contract cost, alternatives, roster needs,
  player projections and information available then.
* State the realistic alternative and why it was better using those contemporaneous facts.
* Address the strongest argument for the decision.
* Explain uncertainty. Do not use subsequent performance as evidence that the original choice was wrong.

Suggested allocation: 40 words of context, 160 words of evidence and alternative, 60 words of
counterargument, 40 words of conclusion. Cite the sources you actually relied on.
