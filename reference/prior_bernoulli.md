# Prior bernoulli distribution

Prior bernoulli distribution

## Usage

``` r
prior_bernoulli(theta)
```

## Arguments

- theta:

  numeric. Probability (in \[0, 1\]).

## Value

Object of class
[`PriorBernoulli`](https://genentech.github.io/psborrow2/reference/PriorBernoulli-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/bernoulli-distribution.html>

## See also

Other priors:
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md),
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md),
[`prior_half_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_half_cauchy.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_normal()`](https://genentech.github.io/psborrow2/reference/prior_normal.md),
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
bp <- prior_bernoulli(0.23)
```
