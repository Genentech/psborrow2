# Prior beta distribution

Prior beta distribution

## Usage

``` r
prior_beta(alpha, beta)
```

## Arguments

- alpha:

  numeric. Shape (\>=0).

- beta:

  numeric. Shape (\>=0).

## Value

Object of class
[`PriorBeta`](https://genentech.github.io/psborrow2/reference/PriorBeta-class.md)

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/beta-distribution.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
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
bp <- prior_beta(9, 235)
```
