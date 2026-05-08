# `PriorGamma` Class

A class for defining gamma priors to be translated to Stan code. Objects
of class `PriorGamma` should not be created directly but by the
constructor
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md).

## Slots

- `stan_code`:

  character. Stan implementation of the prior, with placeholders for
  gamma stan function parameters surrounded with `{{` and `}}` to be
  replaced with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `n_param`:

  integer. Number of prior parameters (2).

- `constraint`:

  character. Support of prior distribution, `"<lower=0>"`.

- `alpha`:

  numeric. Shape (\>0).

- `beta`:

  numeric. Inverse scale (\>=0).

## See also

Other prior classes:
[`Prior-class`](https://genentech.github.io/psborrow2/reference/Prior-class.md),
[`PriorBernoulli-class`](https://genentech.github.io/psborrow2/reference/PriorBernoulli-class.md),
[`PriorBeta-class`](https://genentech.github.io/psborrow2/reference/PriorBeta-class.md),
[`PriorCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorCauchy-class.md),
[`PriorExponential-class`](https://genentech.github.io/psborrow2/reference/PriorExponential-class.md),
[`PriorHalfCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorHalfCauchy-class.md),
[`PriorHalfNormal-class`](https://genentech.github.io/psborrow2/reference/PriorHalfNormal-class.md),
[`PriorNormal-class`](https://genentech.github.io/psborrow2/reference/PriorNormal-class.md),
[`PriorPoisson-class`](https://genentech.github.io/psborrow2/reference/PriorPoisson-class.md),
[`UniformPrior-class`](https://genentech.github.io/psborrow2/reference/UniformPrior-class.md)
