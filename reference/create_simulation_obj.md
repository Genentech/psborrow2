# Compile MCMC sampler using STAN and create simulation object

Compile MCMC sampler using STAN and create simulation object

## Usage

``` r
create_simulation_obj(
  data_matrix_list,
  covariate = NULL,
  outcome,
  borrowing,
  treatment,
  quiet = TRUE
)
```

## Arguments

- data_matrix_list:

  `SimDataList`. The list of lists of data matrices created with
  [`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md).

- covariate:

  `SimCovariateList` or `Covariate` or `NULL`. List of `Covariate`
  objects created with `sim_covariate()`, a single `Covariate` object
  created by
  [`add_covariates()`](https://genentech.github.io/psborrow2/reference/add_covariates.md),
  or `NULL` (no covariate adjustment).

- outcome:

  `SimOutcomeList` or `Outcome`. List of `Outcome` objects created with
  `sim_outcome()`, or single `Outcome` object (e.g., created by
  [`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md)).

- borrowing:

  `SimBorrowingList` or `Borrowing`. List of `Borrowing` objects created
  with `sim_borrowing()`, or a single `Borrowing` object created by
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md),
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md),
  or
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md).

- treatment:

  `SimTreatmentList` or `Treatment`. List of `Treatment` objects created
  with `sim_treatment()` or a single `Treatment` object created by
  [`treatment_details()`](https://genentech.github.io/psborrow2/reference/treatment_details.md).

- quiet:

  logical. Whether to print messages (`quiet = FALSE`) or not
  (`quiet = TRUE`, the default)

## Value

Object of class
[`Simulation`](https://genentech.github.io/psborrow2/reference/Simulation-class.md).

## Examples

``` r
base_mat <- matrix(
  c(
    rep(0, 200), rep(0, 200), rep(1, 200),
    rep(1, 200), rep(0, 200), rep(0, 200),
    rep(0, 600)
  ),
  ncol = 3,
  dimnames = list(NULL, c("ext", "trt", "driftOR"))
)

add_binary_endpoint <- function(odds_ratio,
                                base_matrix = base_mat) {
  linear_predictor <- base_matrix[, "trt"] * log(odds_ratio)
  prob <- 1 / (1 + exp(-linear_predictor))

  bin_endpoint <- rbinom(
    NROW(base_matrix),
    1,
    prob
  )

  cbind(base_matrix, matrix(bin_endpoint, ncol = 1, dimnames = list(NULL, "ep")))
}

data_list <- list(
  list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
  list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
)

guide <- data.frame(
  trueOR = c(1.5, 2.5),
  driftOR = c(1.0, 1.0),
  index = 1:2
)

sdl <- sim_data_list(
  data_list = data_list,
  guide = guide,
  effect = "trueOR",
  drift = "driftOR",
  index = "index"
)

if (check_cmdstan()) {
  sim_object <- create_simulation_obj(
    data_matrix_list = sdl,
    outcome = outcome_bin_logistic("ep", prior_normal(0, 1000)),
    borrowing = sim_borrowing_list(list(
      full_borrowing = borrowing_full("ext"),
      bdb = borrowing_hierarchical_commensurate("ext", prior_exponential(0.0001))
    )),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )
}
```
