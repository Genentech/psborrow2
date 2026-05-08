# Get Simulated Data from `SimDataList` object

Retrieves the simulated data from a `SimDataList` object by index.

## Usage

``` r
get_data(object, index = 1, dataset = 1)

# S4 method for class 'SimDataList'
get_data(object, index = NULL, dataset = NULL)
```

## Arguments

- object:

  `SimDataList` object

- index:

  the index of the scenario (see guide with print(`SimDataList`))

- dataset:

  the dataset out of `n_datasets_per_param`

## Value

Simulated data as a data frame if the index is specified, else as a list
