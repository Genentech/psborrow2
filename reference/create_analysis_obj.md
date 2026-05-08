# Compile MCMC sampler using STAN and create analysis object

Compile MCMC sampler using STAN and create analysis object

## Usage

``` r
create_analysis_obj(
  data_matrix,
  outcome,
  borrowing,
  treatment,
  covariates = NULL,
  quiet = FALSE
)
```

## Arguments

- data_matrix:

  matrix. The data matrix, including all covariates to be adjusted for,
  all relevant outcome variables, and treatment arm and external control
  arm flags.

- outcome:

  `Outcome`. Object of class
  [`Outcome`](https://genentech.github.io/psborrow2/reference/Outcome-class.md)
  as output by
  [`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md),
  [`outcome_surv_weibull_ph()`](https://genentech.github.io/psborrow2/reference/outcome_surv_weibull_ph.md),
  or
  [`outcome_bin_logistic()`](https://genentech.github.io/psborrow2/reference/outcome_bin_logistic.md).

- borrowing:

  `Borrowing`. Object of class
  [`Borrowing`](https://genentech.github.io/psborrow2/reference/Borrowing-class.md)
  as output by
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md),
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md),
  and
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md).

- treatment:

  `Treatment`. Object of class
  [`Treatment`](https://genentech.github.io/psborrow2/reference/Treatment-class.md)
  as output by
  [`treatment_details()`](https://genentech.github.io/psborrow2/reference/treatment_details.md).

- covariates:

  `Covariates`. Object of class
  [`Covariates`](https://genentech.github.io/psborrow2/reference/Covariates-class.md)
  as output by the function
  [`add_covariates()`](https://genentech.github.io/psborrow2/reference/add_covariates.md).

- quiet:

  logical. Whether to suppress messages (`TRUE`) or not (`FALSE`, the
  default)

## Value

Object of class
[`Analysis`](https://genentech.github.io/psborrow2/reference/Analysis-class.md).

## Examples

``` r
if (check_cmdstan()) {
  anls <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      "time",
      "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(.001)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    covariates = add_covariates(
      covariates = c("cov1", "cov2"),
      priors = prior_normal(0, 1000)
    )
  )
}
#> Inputs look good.
#> Stan program compiled successfully!
#> Ready to go! Now call `mcmc_sample()`.
```
