# Create a Fixed External Data Object

Create a Fixed External Data Object

## Usage

``` r
check_fixed_external_data(data, req_cols)
```

## Arguments

- data:

  A `data.frame` containing external control data

- req_cols:

  A `character` vector of required covariate columns

## Value

A `DataSimObject` with updated `enrollment_internal` and
`enrollment_external` slots.
