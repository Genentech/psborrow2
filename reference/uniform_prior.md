# Prior uniform distribution

Prior uniform distribution

## Usage

``` r
uniform_prior(alpha, beta)
```

## Arguments

- alpha:

  numeric. Lower bound.

- beta:

  numeric. Upper bound (\>`alpha`).

## Value

Object of class
[`UniformPrior`](https://genentech.github.io/psborrow2/reference/UniformPrior-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/uniform-distribution.html>

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
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md)

## Examples

``` r
up <- uniform_prior(0, 1)
```
