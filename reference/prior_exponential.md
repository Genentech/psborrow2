# Prior exponential distribution

Prior exponential distribution

## Usage

``` r
prior_exponential(beta)
```

## Arguments

- beta:

  numeric. Inverse scale (\>0).

## Value

Object of class
[`PriorExponential`](https://genentech.github.io/psborrow2/reference/PriorExponential-class.md).

## Details

Stan reference
<https://mc-stan.org/docs/functions-reference/exponential-distribution.html>

## See also

Other priors:
[`prior_bernoulli()`](https://genentech.github.io/psborrow2/reference/prior_bernoulli.md),
[`prior_beta()`](https://genentech.github.io/psborrow2/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_cauchy.md),
[`prior_gamma()`](https://genentech.github.io/psborrow2/reference/prior_gamma.md),
[`prior_half_cauchy()`](https://genentech.github.io/psborrow2/reference/prior_half_cauchy.md),
[`prior_half_normal()`](https://genentech.github.io/psborrow2/reference/prior_half_normal.md),
[`prior_normal()`](https://genentech.github.io/psborrow2/reference/prior_normal.md),
[`prior_poisson()`](https://genentech.github.io/psborrow2/reference/prior_poisson.md),
[`uniform_prior()`](https://genentech.github.io/psborrow2/reference/uniform_prior.md)

## Examples

``` r
ep <- prior_exponential(1)
```
