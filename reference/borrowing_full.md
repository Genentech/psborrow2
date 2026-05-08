# Full borrowing

Full borrowing

## Usage

``` r
borrowing_full(ext_flag_col)
```

## Arguments

- ext_flag_col:

  character. Name of the external flag column in the matrix.

## Value

Object of class
[`BorrowingFull`](https://genentech.github.io/psborrow2/reference/BorrowingFull-class.md).

## Details

### Method

This method does not distinguish between internal and external arms,
effectively pooling patients.

### External Control

The `ext_flag_col` argument refers to the column in the data matrix that
contains the flag indicating a patient is from the external control
cohort.

## See also

Other borrowing:
[`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md)

## Examples

``` r
fb <- borrowing_full("ext")
```
