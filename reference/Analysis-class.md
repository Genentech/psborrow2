# `Analysis` Class

A class for defining Analysis details. Objects of class `Analysis`
should not be created directly but by the constructor
[`create_analysis_obj()`](https://genentech.github.io/psborrow2/reference/create_analysis_obj.md).

## Slots

- `data_matrix`:

  matrix. The data matrix, including all covariates to be adjusted for,
  all relevant outcome variables, and treatment arm and external control
  arm flags.

- `covariates`:

  `Covariate`. Object of class `Covariate` as output by the function
  `covariate_details()`.

- `outcome`:

  `Outcome`. Object of class `Outcome` as output by
  [`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md),
  [`outcome_surv_weibull_ph()`](https://genentech.github.io/psborrow2/reference/outcome_surv_weibull_ph.md),
  or
  [`outcome_bin_logistic()`](https://genentech.github.io/psborrow2/reference/outcome_bin_logistic.md).

- `borrowing`:

  `Borrowing`. Object of class `Borrowing` as output by
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md),
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md),
  or
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md).

- `treatment`:

  `Treatment`. Object of class `Treatment` as output by
  [`treatment_details()`](https://genentech.github.io/psborrow2/reference/treatment_details.md).

- `model_string`:

  character. The string that contains the full Stan model code to be
  compiled.

- `model`:

  `CmdStanModel`. The compiled Stan model as output by
  [`cmdstanr::cmdstan_model()`](https://mc-stan.org/cmdstanr/reference/cmdstan_model.html)

- `ready_to_sample`:

  logical. Is the object ready to sample?
