# `OutcomeContinuousNormal` class

A class for defining a regression with a normal outcome to be translated
to Stan code. Objects of class `OutcomeContinuousNormal` should not be
created directly but by the constructor
[`outcome_cont_normal()`](https://genentech.github.io/psborrow2/reference/outcome_cont_normal.md).

## Slots

- `n_param`:

  integer. Number of ancillary parameters for the model to estimate (0).

- `param_priors`:

  list. Named list of prior distributions on the ancillary parameters in
  the model. Empty for `OutcomeContinuousNormal`.

- `continuous_var`:

  character. Variable used for outcome in `OutcomeContinuousNormal`
  objects.

- `baseline_prior`:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  baseline outcome.

- `name_beta_trt.`:

  Named vector for beta_trt.

- `alpha_type.`:

  How to interpret alpha.

- `name_addnl_params.`:

  Named vector for additional parameters.

## See also

Other outcome:
[`BinaryOutcome-class`](https://genentech.github.io/psborrow2/reference/BinaryOutcome-class.md),
[`ContinuousOutcome-class`](https://genentech.github.io/psborrow2/reference/ContinuousOutcome-class.md),
[`Outcome-class`](https://genentech.github.io/psborrow2/reference/Outcome-class.md),
[`OutcomeBinaryLogistic-class`](https://genentech.github.io/psborrow2/reference/OutcomeBinaryLogistic-class.md),
[`OutcomeSurvExponential-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvExponential-class.md),
[`OutcomeSurvPEM-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvPEM-class.md),
[`OutcomeSurvWeibullPH-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvWeibullPH-class.md),
[`TimeToEvent-class`](https://genentech.github.io/psborrow2/reference/TimeToEvent-class.md)
