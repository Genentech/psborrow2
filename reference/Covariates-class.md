# `Covariate` Class

A class for defining covariate details. Objects of class `Covariate`
should not be created directly but by the constructor
[`add_covariates()`](https://genentech.github.io/psborrow2/reference/add_covariates.md).

## Slots

- `covariates`:

  character. Names of columns in the data matrix containing covariates
  to be adjusted for in the outcome model. Note: the external and
  treatment flags should not go here.

- `priors.`:

  Either a single object of class `Prior` specifying the prior
  distribution to apply to all covariates or a named list of
  distributions of class `Prior`, one for each covariate

- `name_betas.`:

  Names for the beta parameters in the STAN model.
