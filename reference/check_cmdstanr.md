# Check Stan

Check whether `cmdstanr` is available and prints version and logistic
example.

## Usage

``` r
check_cmdstanr(check_sampling = FALSE)

check_cmdstan()
```

## Arguments

- check_sampling:

  Compile and sample from the "logistic" example model.

## Value

`check_cmdstanr()` prints results from checks.

`check_cmdstan()` returns `TRUE` if `CmdStan` seems to be installed,
otherwise `FALSE`

## Functions

- `check_cmdstan()`: Check if the `CmdStan` command line tools are
  available.

## Examples

``` r
check_cmdstanr()
#> cmdstanr is installed.
#> cmdstanr version: 
#> 0.9.0.9000 
#> cmdstan version: 
#> 2.38.0 
#> Example program:
#> data {
#>   int<lower=0> N;
#>   int<lower=0> K;
#>   array[N] int<lower=0, upper=1> y;
#>   matrix[N, K] X;
#> }
#> parameters {
#>   real alpha;
#>   vector[K] beta;
#> }
#> model {
#>   target += normal_lpdf(alpha | 0, 1);
#>   target += normal_lpdf(beta | 0, 1);
#>   target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
#> }
#> generated quantities {
#>   vector[N] log_lik;
#>   for (n in 1 : N) {
#>     log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha + X[n] * beta);
#>   }
#> }
```
