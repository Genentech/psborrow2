# Specify a Time to Event Distribution

Uses [simsurv::simsurv](https://rdrr.io/pkg/simsurv/man/simsurv.html) to
generate time to event data. See `simsurv` help for more details.

## Usage

``` r
create_event_dist(
  dist = NULL,
  lambdas = NULL,
  gammas = NULL,
  mixture = FALSE,
  pmix = 0.5,
  hazard = NULL,
  loghazard = NULL,
  cumhazard = NULL,
  logcumhazard = NULL,
  ...
)

null_event_dist()
```

## Arguments

- dist:

  Specify the distribution `"exponential"`

- lambdas:

  Scale parameter

- gammas:

  Second parameter needed for Weibull or Gompertz distributions

- mixture:

  Use mixture model?

- pmix:

  Proportion of mixtures

- hazard:

  A user defined hazard function

- loghazard:

  Alternatively, a user defined log hazard function

- cumhazard:

  Alternatively, a user defined cumulative hazard function

- logcumhazard:

  Alternatively, a user defined log cumulative hazard function

- ...:

  Other `simsurv` parameters

## Value

A `SimDataEvent` object

`null_event_dist` returns an object with no parameters specified that
does not simulate event times.

## Examples

``` r
weibull_surv <- create_event_dist(dist = "weibull", lambdas = 1 / 200, gammas = 1)
exp_event_dist <- create_event_dist(dist = "exponential", lambdas = 1 / 36)
null_event_dist()
#> An object of class "DataSimEvent"
#> Slot "params":
#> list()
#> 
#> Slot "label":
#> [1] "No distribution specified"
#> 
```
