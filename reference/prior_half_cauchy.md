# Prior half-cauchy distribution

Prior half-cauchy distribution

## Usage

``` r
prior_half_cauchy(mu, sigma)
```

## Arguments

- mu:

  numeric. Location.

- sigma:

  numeric. Scale (\>0).

## Value

Object of class
[`PriorHalfCauchy`](https://genentech.github.io/psborrow2/reference/PriorHalfCauchy-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/cauchy-distribution.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md),
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_normal()`](https://genentech.github.io/psborrow2/reference/prior_normal.md),
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
hcp <- prior_half_cauchy(1, 1)
```
