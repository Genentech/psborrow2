# Create Covariance Matrix

Create Covariance Matrix

## Usage

``` r
covariance_matrix(diag, upper_tri)
```

## Arguments

- diag:

  Diagonal entries of the covariance matrix

- upper_tri:

  Upper triangle entries of the matrix, specified column wise.

## Value

A symmetric matrix with `diag` values on the main diagonal and
`upper_tri` values in the lower and upper triangles.

## Examples

``` r
m1 <- covariance_matrix(c(1, 1, 1, 1), c(.8, .3, .8, 0, 0, 0))
m1
#>      [,1] [,2] [,3] [,4]
#> [1,]  1.0  0.8  0.3    0
#> [2,]  0.8  1.0  0.8    0
#> [3,]  0.3  0.8  1.0    0
#> [4,]  0.0  0.0  0.0    1
mvtnorm::rmvnorm(5, mean = c(0, 0, 0, 0), sigma = m1)
#>            [,1]        [,2]       [,3]         [,4]
#> [1,] -1.1598392 -1.72881742 -2.0276398 -0.005571287
#> [2,]  1.0555399  0.21815984 -0.9887304 -0.247325302
#> [3,] -0.3690693 -0.59796639 -0.6280154  0.628982042
#> [4,]  0.9934706  0.12464224 -0.3055261 -1.863011492
#> [5,] -0.4622994 -0.02691387  0.4287509 -0.914074827

# No correlation
covariance_matrix(c(1, 2, 3))
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    2    0
#> [3,]    0    0    3
```
