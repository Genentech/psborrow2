# Get Quantiles of Random Data

Helper for use within transformation functions for
[`create_baseline_object()`](https://genentech.github.io/psborrow2/reference/create_baseline_object.md).

## Usage

``` r
get_quantiles(object, var)
```

## Arguments

- object:

  a `BaselineDataFrame`

- var:

  character string name of the variable

## Value

A numeric vector containing quantiles based on the data generating
distribution.
