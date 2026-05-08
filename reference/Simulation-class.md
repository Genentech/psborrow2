# `Simulation` Class

A class for defining Simulation study details. Objects of class
`Simulation` should not be created directly but by the constructor
[`create_simulation_obj()`](https://genentech.github.io/psborrow2/reference/create_simulation_obj.md).

## Slots

- `data_matrix_list`:

  `SimDataList`. The list of lists of data matrices created with
  [`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md).

- `outcome`:

  `SimOutcomeList`. List of `Outcome` objects created with
  [`sim_outcome_list()`](https://genentech.github.io/psborrow2/reference/sim_outcome_list.md).

- `borrowing`:

  `SimBorrowingList`. List of `Borrowing` objects created with
  [`sim_borrowing_list()`](https://genentech.github.io/psborrow2/reference/sim_borrowing_list.md).

- `covariate`:

  `SimCovariateList` or `NULL`. List of `Covariate` objects created with
  [`sim_covariate_list()`](https://genentech.github.io/psborrow2/reference/sim_covariate_list.md)
  or `NULL` (no covariate adjustment).

- `treatment`:

  `SimTreatmentList`. List of `Treatment` objects created with
  [`sim_treatment_list()`](https://genentech.github.io/psborrow2/reference/sim_treatment_list.md).

- `guide`:

  data.frame. Data.frame containing information on all combinations
  evaluated.

- `n_combos`:

  integer. Number of combinations of parameters to be evaluated.

- `n_analyses`:

  integer. Number of analyses (combos x datasets to be performed).

- `` `analysis_obj_list` ``:

  list. List of analysis objects indexed according to `guide`.
