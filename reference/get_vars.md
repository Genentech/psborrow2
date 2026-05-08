# Get Variables

Gets the data variable names from an object.

## Usage

``` r
get_vars(object)

# S4 method for class 'Covariates'
get_vars(object)

# S4 method for class 'Treatment'
get_vars(object)

# S4 method for class 'Borrowing'
get_vars(object)

# S4 method for class 'TimeToEvent'
get_vars(object)

# S4 method for class 'BinaryOutcome'
get_vars(object)

# S4 method for class 'ContinuousOutcome'
get_vars(object)

# S4 method for class 'Analysis'
get_vars(object)

# S4 method for class 'NULL'
get_vars(object)

# S4 method for class 'BorrowingFixedPowerPrior'
get_vars(object)

# S4 method for class 'SimTreatmentList'
get_vars(object)

# S4 method for class 'SimOutcomeList'
get_vars(object)

# S4 method for class 'SimBorrowingList'
get_vars(object)

# S4 method for class 'SimCovariateList'
get_vars(object)

# S4 method for class 'Simulation'
get_vars(object)

# S4 method for class 'BaselineObject'
get_vars(object)
```

## Arguments

- object:

  Object

## Value

A `character` vector containing variable names

## Examples

``` r
get_vars(treatment_details(
  trt_flag_col = "treat_fl",
  trt_prior = prior_normal(0, 1000)
))
#> trt_flag_col 
#>   "treat_fl" 
```
