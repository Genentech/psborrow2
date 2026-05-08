# Prior normal distribution

Prior normal distribution

## Usage

``` r
prior_normal(mu, sigma)
```

## Arguments

- mu:

  numeric. Location.

- sigma:

  numeric. Scale (\>0).

## Value

Object of class
[`PriorNormal`](https://genentech.github.io/psborrow2/reference/PriorNormal-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/normal-distribution.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md),
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md),
[`prior_half_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_half_cauchy.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
np <- prior_normal(1, 1)
```
