# Input generated data for a simulation study

A function for defining generated data for use as part of a simulation
study.

## Usage

``` r
sim_data_list(data_list, guide, effect, drift, index)
```

## Arguments

- data_list:

  list of lists of matrices. The lists at the highest level differ in
  that the parameters used to generate the data. The matrices at lowest
  level are different iterations of the same data generation parameters.
  See `details`.

- guide:

  data.frame. `guide` contains information on the parameters that differ
  at the highest level of `data_list`. See `details.`

- effect:

  character. The column in `guide` that corresponds to the true
  treatment effect estimate (hazard ratio or odds ratio).

- drift:

  character. The column in `guide` that corresponds to the true drift
  effect estimate (hazard ratio or odds ratio). A drift \>1 means the
  external arm experiences greater effects.

- index:

  character. The column in `guide` that corresponds to the index column.

## Value

Object of class
[`SimDataList`](https://genentech.github.io/psborrow2/reference/SimDataList-class.md).

## Details

In this function, you are providing generated data for analysis in a
simulation study in `psborrow2`. Note that this function does not do any
data generation on your behalf; it assumes that you have generated the
data already. For a full working example, refer to the relevant
vignette:
[`vignette('simulation_study', package = 'psborrow2')`](https://genentech.github.io/psborrow2/articles/simulation_study.md).

More information on the inputs is provided below.

### Matrix requirements in `data_list`

Each matrix embedded in `data_list` must have:

1.  a flag for whether the patient is an external control

2.  a flag for whether the patient is in the experimental treatment arm

3.  outcome information (time and censorship for survival, flag for
    outcome in binary endpoints)

Optionally, the matrices may also contain covariates. See `examples`.

### `data_list`

Each set of distinct data generation parameters should be represented by
a single list of matrices. Because multiple scenarios may want to be
compared, a list of list of matrices is preferred. See `examples`.

### `guide`

The `guide` should be a data.frame with one row per scenario. As a
consquence of this, the length of the list should equal the number of
rows in the guide. See `examples`.

## See also

Other simulation classes:
[`sim_borrowing_list()`](https://genentech.github.io/psborrow2/reference/sim_borrowing_list.md),
[`sim_covariate_list()`](https://genentech.github.io/psborrow2/reference/sim_covariate_list.md),
[`sim_outcome_list()`](https://genentech.github.io/psborrow2/reference/sim_outcome_list.md),
[`sim_treatment_list()`](https://genentech.github.io/psborrow2/reference/sim_treatment_list.md)

## Examples

``` r
base_mat <- matrix(
  c(
    rep(0, 200), rep(0, 200), rep(1, 200),
    rep(1, 200), rep(0, 200), rep(0, 200),
    rep(0, 600)
  ),
  ncol = 3,
  dimnames = list(NULL, c("ext", "trt", "driftOR"))
)

add_binary_endpoint <- function(odds_ratio,
                                base_matrix = base_mat) {
  linear_predictor <- base_matrix[, "trt"] * log(odds_ratio)
  prob <- 1 / (1 + exp(-linear_predictor))

  bin_endpoint <- rbinom(
    NROW(base_matrix),
    1,
    prob
  )

  cbind(base_matrix, matrix(bin_endpoint, ncol = 1, dimnames = list(NULL, "ep")))
}

data_list <- list(
  list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
  list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
)

guide <- data.frame(
  trueOR = c(1.5, 2.5),
  driftOR = c(1.0, 1.0),
  ind = c(1, 2)
)

sdl <- sim_data_list(
  data_list = data_list,
  guide = guide,
  effect = "trueOR",
  drift = "driftOR",
  index = "ind"
)
```
