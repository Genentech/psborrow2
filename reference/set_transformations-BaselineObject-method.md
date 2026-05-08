# Set Transformations in Baseline Objects

Set Transformations in Baseline Objects

## Usage

``` r
# S4 method for class 'BaselineObject'
set_transformations(object, ..., overwrite = FALSE)
```

## Arguments

- object:

  `BaselineObject` created by
  [create_baseline_object](https://genentech.github.io/psborrow2/reference/create_baseline_object.md).

- ...:

  named transformation functions. See details for more information.

- overwrite:

  If `TRUE` overwrite existing transformation list and only include
  newly specified transformations.

## Value

An updated `BaselineObject`

## Details

Transformation functions are evaluated in order and create or overwrite
a column in the data.frame with that name. The function should have
signature `function(data)`, taking a `data.frame` (specifically a
`BaselineDataFrame` object from `generate(BaselineObject)`) and return a
vector with length identical to the total number of patients. The
`@BaselineObject` slot of the
[BaselineDataFrame](https://genentech.github.io/psborrow2/reference/BaselineDataFrame-class.md)
may be accessed directly or with
[`get_quantiles()`](https://genentech.github.io/psborrow2/reference/get_quantiles.md)
to create transformations. See
[`binary_cutoff()`](https://genentech.github.io/psborrow2/reference/binary_cutoff.md).

## Examples

``` r
baseline <- create_baseline_object(
  100, 50, 100,
  covariates = baseline_covariates(
    names = "age", means_int = 55,
    covariance_int = covariance_matrix(5)
  )
)
set_transformations(baseline, age_scaled = function(data) scale(data$age))
#> Baseline Data Simulation Object
#>   N internal treated:  100 
#>   N internal control:  50 
#>   N external control:  100 
#> 
#> Covariates: 
#> [[1]]
#>  covariate means_internal means_external
#>        age             55             55
#> 
#> Covariance Matrices
#> Internal   External
#>     age        age 
#> age   5    age   5 
#> 
#> Transformations: 
#>    age_scaled 
```
