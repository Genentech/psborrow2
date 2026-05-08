# `UniformPrior` Class

A class for defining uniform priors to be translated to Stan code.
Objects of class `UniformPrior` should not be created directly but by
the constructor
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md).

## Slots

- `stan_code`:

  character. Stan implementation of the prior, with placeholders for
  uniform stan function parameters surrounded with `{{` and `}}` to be
  replaced with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `n_param`:

  integer. Number of prior parameters (2).

- `constraint`:

  character. Support of prior distribution,
  `"<lower=`alpha`, upper=`beta`>"`.

- `alpha`:

  numeric. Lower bound.

- `beta`:

  numeric. Upper bound (\>`alpha`).

## See also

Other prior classes:
[`Prior-class`](https://genentech.github.io/psborrow2/reference/Prior-class.md),
[`PriorBernoulli-class`](https://genentech.github.io/psborrow2/reference/PriorBernoulli-class.md),
[`PriorBeta-class`](https://genentech.github.io/psborrow2/reference/PriorBeta-class.md),
[`PriorCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorCauchy-class.md),
[`PriorExponential-class`](https://genentech.github.io/psborrow2/reference/PriorExponential-class.md),
[`PriorGamma-class`](https://genentech.github.io/psborrow2/reference/PriorGamma-class.md),
[`PriorHalfCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorHalfCauchy-class.md),
[`PriorHalfNormal-class`](https://genentech.github.io/psborrow2/reference/PriorHalfNormal-class.md),
[`PriorNormal-class`](https://genentech.github.io/psborrow2/reference/PriorNormal-class.md),
[`PriorPoisson-class`](https://genentech.github.io/psborrow2/reference/PriorPoisson-class.md)
