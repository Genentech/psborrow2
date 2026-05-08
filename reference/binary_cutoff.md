# Binary Cut-Off Transformation

Binary Cut-Off Transformation

## Usage

``` r
binary_cutoff(name, int_cutoff, ext_cutoff)
```

## Arguments

- name:

  variable to transform

- int_cutoff:

  cut-off for internal patients, numeric between 0 and 1

- ext_cutoff:

  cut-off for external patients, numeric between 0 and 1

## Value

Transformation function to be used in
[`create_baseline_object()`](https://genentech.github.io/psborrow2/reference/create_baseline_object.md).
Sets quantile values larger than cut-off value to `TRUE` otherwise
`FALSE`.

## Examples

``` r
# Creates a simple function, where `data` is a `BaselineDataFrame`:
function(data) {
  ext <- data$ext == 0
  q <- get_quantiles(data, name)
  ifelse(ext, q > int_cutoff, q > ext_cutoff)
}
#> function (data) 
#> {
#>     ext <- data$ext == 0
#>     q <- get_quantiles(data, name)
#>     ifelse(ext, q > int_cutoff, q > ext_cutoff)
#> }
#> <environment: 0x555bac78d2a0>
```
