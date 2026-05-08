# `PriorExponential` Class

A class for defining exponential priors to be translated to Stan code.
Objects of class `PriorExponential` should not be created directly but
by the constructor
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md).

## Slots

- `stan_code`:

  character. Stan implementation of the prior, with placeholders for
  exponential Stan function parameters surrounded with `{{` and `}}` to
  be replaced with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `n_param`:

  integer. Number of prior parameters (1).

- `constraint`:

  character. Support of prior distribution, `"<lower=0>"`.

- `beta`:

  numeric. Inverse scale (\>0).

## See also

Other prior classes:
[`Prior-class`](https://genentech.github.io/psborrow2/reference/Prior-class.md),
[`PriorBernoulli-class`](https://genentech.github.io/psborrow2/reference/PriorBernoulli-class.md),
[`PriorBeta-class`](https://genentech.github.io/psborrow2/reference/PriorBeta-class.md),
[`PriorCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorCauchy-class.md),
[`PriorGamma-class`](https://genentech.github.io/psborrow2/reference/PriorGamma-class.md),
[`PriorHalfCauchy-class`](https://genentech.github.io/psborrow2/reference/PriorHalfCauchy-class.md),
[`PriorHalfNormal-class`](https://genentech.github.io/psborrow2/reference/PriorHalfNormal-class.md),
[`PriorNormal-class`](https://genentech.github.io/psborrow2/reference/PriorNormal-class.md),
[`PriorPoisson-class`](https://genentech.github.io/psborrow2/reference/PriorPoisson-class.md),
[`UniformPrior-class`](https://genentech.github.io/psborrow2/reference/UniformPrior-class.md)
