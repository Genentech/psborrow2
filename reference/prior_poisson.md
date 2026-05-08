# Prior poisson distribution

Prior poisson distribution

## Usage

``` r
prior_poisson(lambda)
```

## Arguments

- lambda:

  numeric. Rate (\>0).

## Value

Object of class
[`PriorPoisson`](https://genentech.github.io/psborrow2/reference/PriorPoisson-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/poisson.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md),
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md),
[`prior_half_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_half_cauchy.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_normal()`](https://genentech.github.io/psborrow2/reference/prior_normal.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
pp <- prior_poisson(100)
```
