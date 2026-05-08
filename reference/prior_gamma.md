# Prior gamma distribution

Prior gamma distribution

## Usage

``` r
prior_gamma(alpha, beta)
```

## Arguments

- alpha:

  numeric. Shape (\>0).

- beta:

  numeric. Inverse scale (\>=0).

## Value

Object of class
[`PriorGamma`](https://genentech.github.io/psborrow2/reference/PriorGamma-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/gamma-distribution.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_exponential()`](https://genentech.github.io/psborrow2/reference/prior_exponential.md),
[`prior_half_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_half_cauchy.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_normal()`](https://genentech.github.io/psborrow2/reference/prior_normal.md),
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
gp <- prior_gamma(0.001, 0.001)
```
