# Rename Covariates in `draws` Object

Rename Covariates in `draws` Object

## Usage

``` r
rename_draws_covariates(draws, analysis)
```

## Arguments

- draws:

  `draws` created from sampled analysis object. See example.

- analysis:

  `Analysis` as created by
  [`create_analysis_obj()`](https://genentech.github.io/psborrow2/reference/create_analysis_obj.md).

## Value

A
`draws`\[[posterior::draws](https://mc-stan.org/posterior/reference/draws.html)\]
object with covariate names.

## Examples

``` r
if (check_cmdstan()) {
  analysis_object <- create_analysis_obj(
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
  samples <- mcmc_sample(analysis_object, 200, 400, 1)
  draws <- samples$draws()
  renamed_draws <- rename_draws_covariates(draws, analysis_object)
  summary(renamed_draws)
}
#> Inputs look good.
#> Stan program compiled successfully!
#> Ready to go! Now call `mcmc_sample()`.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 0.4 seconds.
#> # A tibble: 8 × 10
#>   variable        mean   median      sd     mad       q5      q95  rhat ess_bulk
#>   <chr>          <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#> 1 lp__        -1.57e+3 -1.57e+3 1.66e+0  1.48   -1.57e+3 -1.56e+3 1.00     146. 
#> 2 treatment … -5.68e-1 -5.75e-1 2.35e-1  0.218  -9.51e-1 -1.76e-1 1.01      95.4
#> 3 baseline l… -3.71e+0 -3.70e+0 1.98e-1  0.200  -4.04e+0 -3.38e+0 0.999    113. 
#> 4 baseline l… -3.21e+0 -3.20e+0 1.26e-1  0.118  -3.43e+0 -3.01e+0 1.01     185. 
#> 5 commensura…  7.29e+1  1.07e+1 2.39e+2 11.2     1.29e+0  4.34e+2 1.000    101. 
#> 6 cov1         8.18e-1  8.23e-1 1.15e-1  0.115   6.19e-1  9.99e-1 1.00     252. 
#> 7 cov2         5.71e-1  5.70e-1 9.69e-2  0.0972  4.13e-1  7.35e-1 1.03     302. 
#> 8 treatment …  5.82e-1  5.63e-1 1.39e-1  0.123   3.86e-1  8.39e-1 1.01      95.4
#> # ℹ 1 more variable: ess_tail <dbl>
```
