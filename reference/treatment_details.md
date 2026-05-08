# Specify Treatment Details

Specify the treatment arm column name in the model matrix and set a
prior distribution for the treatment effect (log hazard ratio or log
odds ratio)

## Usage

``` r
treatment_details(trt_flag_col, trt_prior)
```

## Arguments

- trt_flag_col:

  character. The name of the column in the model matrix that corresponds
  to the treatment flag (`1`/`0` or `TRUE`/`FALSE`). This identifies
  patients as belonging to the experimental treatment arm.

- trt_prior:

  Object of class `Prior` specifying the prior distribution of the log
  effect estimate (log hazard ratio for time to event endpoints and log
  odds ratio for binary endpoints).

## Value

Object of class
[`Treatment`](https://genentech.github.io/psborrow2/reference/Treatment-class.md).

## Examples

``` r
sta <- treatment_details(
  trt_flag_col = "trt",
  trt_prior = prior_normal(0, 1000)
)
```
