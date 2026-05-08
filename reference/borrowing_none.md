# No borrowing

No borrowing

## Usage

``` r
borrowing_none(ext_flag_col)
```

## Arguments

- ext_flag_col:

  character. Name of the external flag column in the matrix.

## Value

Object of class
[`BorrowingNone`](https://genentech.github.io/psborrow2/reference/BorrowingNone-class.md).

## Details

### Method

This method evaluates only the internal comparison, ignoring historical
controls. Note that this method will filter the model matrix based on
values in `ext_flag_col`.

### External Control

The `ext_flag_col` argument refers to the column in the data matrix that
contains the flag indicating a patient is from the external control
cohort.

## See also

Other borrowing:
[`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md)

## Examples

``` r
db <- borrowing_none(
  ext_flag_col = "ext"
)
```
