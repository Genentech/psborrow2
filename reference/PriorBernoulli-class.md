# `PriorBernoulli` Class

A class for defining bernoulli priors to be translated to Stan code.
Objects of class `PriorBernoulli` should not be created directly but by
the constructor
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md).

## Slots

- `stan_code`:

  character. Stan implementation of the prior, with placeholders for
  bernoulli stan function parameters surrounded with `{{` and `}}` to be
  replaced with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `n_param`:

  integer. Number of prior parameters (1).

- `constraint`:

  character. Support of prior distribution, `"<lower=0, upper=1>"`.

- `theta`:

  numeric. Probability (in \[0, 1\]).

## See also

Other prior classes:
[`Prior-class`](https://genentech.github.io/psborrow2/reference/Prior-class.md),
[`PriorBeta-class`](https://genentech.github.io/psborrow2/reference/PriorBeta-class.md),
[`PriorCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorCauchy-class.md),
[`PriorExponential-class`](https://genentech.github.io/psborrow2/reference/PriorExponential-class.md),
[`PriorGamma-class`](https://genentech.github.io/psborrow2/reference/PriorGamma-class.md),
[`PriorHalfCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorHalfCauchy-class.md),
[`PriorHalfNormal-class`](https://genentech.github.io/psborrow2/reference/PriorHalfNormal-class.md),
[`PriorNormal-class`](https://genentech.github.io/psborrow2/reference/PriorNormal-class.md),
[`PriorPoisson-class`](https://genentech.github.io/psborrow2/reference/PriorPoisson-class.md),
[`UniformPrior-class`](https://genentech.github.io/psborrow2/reference/UniformPrior-class.md)
