# Bernoulli distribution with logit parametrization

Bernoulli distribution with logit parametrization

## Usage

``` r
outcome_bin_logistic(binary_var, baseline_prior, weight_var = "")
```

## Arguments

- binary_var:

  character. Name of binary (1/0 or TRUE/FALSE) outcome variable in the
  model matrix

- baseline_prior:

  `Prior`. Object of class `Prior` specifying prior distribution for the
  baseline outcome. See `Details` for more information.

- weight_var:

  character. Optional name of variable in model matrix for weighting the
  log likelihood.

## Value

Object of class
[`OutcomeBinaryLogistic`](https://genentech.github.io/psborrow2/reference/OutcomeBinaryLogistic-class.md).

## Details

### Baseline Prior

The `baseline_prior` argument specifies the prior distribution for the
baseline log odds. The interpretation of the `baseline_prior` differs
slightly between borrowing methods selected.

- *Dynamic borrowing using
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md)*:
  the `baseline_prior` for Bayesian Dynamic Borrowing refers to the log
  odds of the external control arm.

- *Full borrowing* or *No borrowing* using
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md)
  or
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md):
  the `baseline_prior` for these borrowing methods refers to the log
  odds for the internal control arm.

## See also

Other outcome models:
[`outcome_cont_normal()`](https://genentech.github.io/psborrow2/reference/outcome_cont_normal.md),
[`outcome_surv_exponential()`](https://genentech.github.io/psborrow2/reference/outcome_surv_exponential.md),
[`outcome_surv_pem()`](https://genentech.github.io/psborrow2/reference/outcome_surv_pem.md),
[`outcome_surv_weibull_ph()`](https://genentech.github.io/psborrow2/reference/outcome_surv_weibull_ph.md)

## Examples

``` r
lg <- outcome_bin_logistic(
  binary_var = "response",
  baseline_prior = prior_normal(0, 1000)
)
```
