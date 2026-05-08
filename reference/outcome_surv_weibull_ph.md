# Weibull survival distribution (proportional hazards formulation)

Weibull survival distribution (proportional hazards formulation)

## Usage

``` r
outcome_surv_weibull_ph(
  time_var,
  cens_var,
  shape_prior,
  baseline_prior,
  weight_var = ""
)
```

## Arguments

- time_var:

  character. Name of time variable column in model matrix

- cens_var:

  character. Name of the censorship variable flag in model matrix

- shape_prior:

  `Prior` class object for the Weibull shape parameter. Default is
  `prior_exponential(beta = 0.0001)`.

- baseline_prior:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  baseline outcome. See `Details` for more information.

- weight_var:

  character. Optional name of variable in model matrix for weighting the
  log likelihood.

## Value

Object of class
[`OutcomeSurvWeibullPH`](https://genentech.github.io/psborrow2/reference/OutcomeSurvWeibullPH-class.md).

## Details

### Baseline Prior

The `baseline_prior` argument specifies the prior distribution for the
baseline log hazard rate. The interpretation of the `baseline_prior`
differs slightly between borrowing methods selected.

- *Dynamic borrowing using
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md)*:
  the `baseline_prior` for Bayesian Dynamic Borrowing refers to the log
  hazard rate of the external control arm.

- *Full borrowing* or *No borrowing* using
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md)
  or
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md):
  the `baseline_prior` for these borrowing methods refers to the log
  hazard rate for the internal control arm.

## See also

Other outcome models:
[`outcome_bin_logistic()`](https://genentech.github.io/psborrow2/reference/outcome_bin_logistic.md),
[`outcome_cont_normal()`](https://genentech.github.io/psborrow2/reference/outcome_cont_normal.md),
[`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md),
[`outcome_surv_pem()`](https://genentech.github.io/psborrow2/reference/outcome_surv_pem.md)

## Examples

``` r
ws <- outcome_surv_weibull_ph(
  time_var = "time",
  cens_var = "cens",
  shape_prior = prior_exponential(1),
  baseline_prior = prior_normal(0, 1000)
)
```
