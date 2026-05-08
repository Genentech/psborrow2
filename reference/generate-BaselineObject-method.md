# Generate Data for a `BaselineObject`

Generate Data for a `BaselineObject`

## Usage

``` r
# S4 method for class 'BaselineObject'
generate(x, ...)
```

## Arguments

- x:

  a `BaselineObject` object created by
  [create_baseline_object](https://genentech.github.io/psborrow2/reference/create_baseline_object.md)

- ...:

  additional parameters are ignored

## Value

A
[BaselineDataFrame](https://genentech.github.io/psborrow2/reference/BaselineDataFrame-class.md)
object

## Examples

``` r
bl_biomarkers <- create_baseline_object(
  n_trt_int = 100,
  n_ctrl_int = 50,
  n_ctrl_ext = 100,
  covariates = baseline_covariates(
    c("b1", "b2", "b3"),
    means_int = c(0, 0, 0),
    covariance_int = covariance_matrix(c(1, 1, 1), c(.8, .3, .8))
  ),
  transformations = list(
    exp_b1 = function(data) exp(data$b1),
    b2 = binary_cutoff("b2", int_cutoff = 0.7, ext_cutoff = 0.5)
  )
)
generate(bl_biomarkers)
#> Baseline Data List
#> 
#> Internal Treated
#>  patid ext trt         b1 b2         b3    exp_b1
#>      1   0   1  1.9868469  1 -0.9774339 7.2925034
#>      2   0   1 -0.5626329  0 -0.8197433 0.5697071
#>      3   0   1  1.7877273  1  0.4926398 5.9758555
#>      4   0   1  0.3868788  0 -0.6389805 1.4723780
#>      5   0   1 -0.1674349  1  1.2243768 0.8458316
#>      6   0   1 -0.2388432  0  0.1933471 0.7875384
#> Internal Control
#>  patid ext trt          b1 b2          b3    exp_b1
#>    101   0   0  0.91163631  0 -1.23328855 2.4883910
#>    102   0   0 -0.05962584  0  0.01036414 0.9421170
#>    103   0   0 -1.74516314  0 -1.87673235 0.1746165
#>    104   0   0 -0.71453896  0 -0.89547245 0.4894177
#>    105   0   0  1.14064393  0 -1.26310156 3.1287824
#>    106   0   0  1.25791955  1  1.04905530 3.5180947
#> External Control
#>  patid ext trt          b1 b2          b3    exp_b1
#>    151   1   0 -0.60944092  0 -0.19344395 0.5436547
#>    152   1   0 -0.81619792  0 -0.19270257 0.4421094
#>    153   1   0 -1.69887264  0 -0.12565894 0.1828896
#>    154   1   0  0.92772925  1  0.50089527 2.5287605
#>    155   1   0 -0.11833091  0 -0.01742130 0.8884020
#>    156   1   0 -0.01166284  0  0.08529111 0.9884049
```
