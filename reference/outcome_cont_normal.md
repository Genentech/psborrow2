# Normal Outcome Distribution

Normal Outcome Distribution

## Usage

``` r
outcome_cont_normal(
  continuous_var,
  baseline_prior,
  std_dev_prior,
  weight_var = ""
)
```

## Arguments

- continuous_var:

  character. Name of continuous outcome variable in the model matrix

- baseline_prior:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  baseline outcome. See `Details` for more information.

- std_dev_prior:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  standard deviation of the outcome distribution (i.e. "sigma").

- weight_var:

  character. Optional name of variable in model matrix for weighting the
  log likelihood.

## Value

Object of class
[`OutcomeContinuousNormal`](https://genentech.github.io/psborrow2/reference/OutcomeContinuousNormal-class.md).

## Details

### Baseline Prior

The `baseline_prior` argument specifies the prior distribution for the
intercept of the linear model. The interpretation of the
`baseline_prior` differs slightly between borrowing methods selected.

- *Dynamic borrowing using
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md)*:
  the `baseline_prior` for Bayesian Dynamic Borrowing refers to the
  intercept of the external control arm.

- *Full borrowing* or *No borrowing* using
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md)
  or
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md):
  the `baseline_prior` for these borrowing methods refers to the
  intercept for the internal control arm.

## See also

Other outcome models:
[`outcome_bin_logistic()`](https://genentech.github.io/psborrow2/reference/outcome_bin_logistic.md),
[`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md),
[`outcome_surv_pem()`](https://genentech.github.io/psborrow2/reference/outcome_surv_pem.md),
[`outcome_surv_weibull_ph()`](https://genentech.github.io/psborrow2/reference/outcome_surv_weibull_ph.md)

## Examples

``` r
norm <- outcome_cont_normal(
  continuous_var = "tumor_size",
  baseline_prior = prior_normal(0, 100),
  std_dev_prior = prior_half_cauchy(1, 5)
)
```
