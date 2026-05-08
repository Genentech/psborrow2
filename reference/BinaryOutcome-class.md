# `BinaryOutcome` class

`BinaryOutcome` class

## Slots

- `n_param`:

  integer. Number of ancillary parameters for the model to estimate.

- `param_priors`:

  list. Named list of prior distributions on the ancillary parameters in
  the model.

- `binary_var`:

  character. Variable used for outcome in `BinaryOutcome` objects.

- `baseline_prior`:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  baseline outcome.

- `name_beta_trt.`:

  Named vector for beta_trt.

- `name_exp_trt.`:

  Named vector for exponentiated beta_trt

- `alpha_type.`:

  How to interpret alpha.

- `name_addnl_params.`:

  Named vector for additional parameters.

## See also

Other outcome:
[`ContinuousOutcome-class`](https://genentech.github.io/psborrow2/reference/ContinuousOutcome-class.md),
[`Outcome-class`](https://genentech.github.io/psborrow2/reference/Outcome-class.md),
[`OutcomeBinaryLogistic-class`](https://genentech.github.io/psborrow2/reference/OutcomeBinaryLogistic-class.md),
[`OutcomeContinuousNormal-class`](https://genentech.github.io/psborrow2/reference/OutcomeContinuousNormal-class.md),
[`OutcomeSurvExponential-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvExponential-class.md),
[`OutcomeSurvPEM-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvPEM-class.md),
[`OutcomeSurvWeibullPH-class`](https://genentech.github.io/psborrow2/reference/OutcomeSurvWeibullPH-class.md),
[`TimeToEvent-class`](https://genentech.github.io/psborrow2/reference/TimeToEvent-class.md)
