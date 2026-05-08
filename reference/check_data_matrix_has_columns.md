# Check Data Matrix for Required Columns

Check that an `Analysis` object's `data_matrix` has all the required
variables.

## Usage

``` r
check_data_matrix_has_columns(object)
```

## Arguments

- object:

  `Analysis`. Object to check.

## Value

[`stop()`](https://rdrr.io/r/base/stop.html) if some columns are
missing.

## Examples

``` r
anls <- create_analysis_obj(
  data_matrix = example_matrix,
  covariates = add_covariates(
    covariates = c("cov1", "cov2"),
    priors = prior_normal(0, 1000)
  ),
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
  )
)
#> Inputs look good.
#> Stan program compiled successfully!
#> Ready to go! Now call `mcmc_sample()`.

check_data_matrix_has_columns(anls)
```
