# Piecewise exponential survival distribution

Piecewise exponential survival distribution

## Usage

``` r
outcome_surv_pem(
  time_var,
  cens_var,
  baseline_prior,
  weight_var = "",
  cut_points
)
```

## Arguments

- time_var:

  character. Name of time variable column in model matrix

- cens_var:

  character. Name of the censorship variable flag in model matrix

- baseline_prior:

  `Prior`. Object of class `Prior` specifying prior distribution for
  each cut point. See `Details` for more information.

- weight_var:

  character. Optional name of variable in model matrix for weighting the
  log likelihood.

- cut_points:

  numeric. Vector of internal cut points for the piecewise exponential
  model. Note: the choice of cut points will impact the amount of
  borrowing between arms when dynamic borrowing methods are selected. It
  is recommended to choose cut points that contain an equal number of
  events within each interval. Please include only internal cut points
  in the vector. For instance, for cut points of \[0, 15\], (15, 20\],
  (20, Inf), the vector should be c(15, 20). If you pass cut-points
  beyond the follow-up of the data, you will receive an informative
  warning when calling `create_analysis_object()` and these cut points
  will be ignored.

## Value

Object of class
[`OutcomeSurvPEM`](https://genentech.github.io/psborrow2/reference/OutcomeSurvPEM-class.md).

## Details

### Baseline Prior

The `baseline_prior` argument specifies the prior distribution for the
baseline log hazard rate within each cutpoint. Currently, there is no
option to consider different baseline priors within each cut point. The
interpretation of the `baseline_prior` differs slightly between
borrowing methods selected.

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
[`outcome_surv_weibull_ph()`](https://genentech.github.io/psborrow2/reference/outcome_surv_weibull_ph.md)

## Examples

``` r
es <- outcome_surv_pem(
  time_var = "time",
  cens_var = "cens",
  baseline_prior = prior_normal(0, 1000),
  cut_points = c(10, 15, 30)
)
```
