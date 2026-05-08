# Add Covariates for Model Adjustment

Specify column names for adjustment variables in model matrix and prior
distributions for the model parameters for these covariates (i.e.,
betas)

## Usage

``` r
add_covariates(covariates, priors)
```

## Arguments

- covariates:

  character. Names of columns in the data matrix containing covariates
  to be adjusted for in the outcome model. Note: the external and
  treatment flags should not go here.

- priors:

  Either a single object of class `Prior` specifying the prior
  distribution to apply to all covariates or a named list of
  distributions of class `Prior`, one for each covariate

## Value

Object of class
[`Covariates`](https://genentech.github.io/psborrow2/reference/Covariates-class.md).

## Examples

``` r
add_covariates(
  covariates = c("a", "b"),
  priors = list(
    "a" = prior_normal(0, 1),
    "b" = prior_normal(0, 2)
  )
)
#> Covariate object with priors for variables:
#> a, b 
#> 
#> Prior for a:
#> Normal Distribution
#> Parameters:
#>  Stan  R    Value
#>  mu    mean 0    
#>  sigma sd   1    
#> 
#> Prior for b:
#> Normal Distribution
#> Parameters:
#>  Stan  R    Value
#>  mu    mean 0    
#>  sigma sd   2    
#> 
```
