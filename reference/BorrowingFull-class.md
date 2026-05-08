# `BorrowingFull` class

A class for defining details for "Full Borrowing" methods. Objects of
class `BorrowingFull` should not be created directly but by the
constructor
[`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md).

## Slots

- `method_name`:

  string. The name of the method.

- `ext_flag_col`:

  character. Name of the external flag column in the matrix.

- `name_tau`:

  named vector for hierarchical commensurability parameter hyperprior.

## See also

Other borrowing classes:
[`Borrowing-class`](https://genentech.github.io/psborrow2/reference/Borrowing-class.md),
[`BorrowingFixedPowerPrior-class`](https://genentech.github.io/psborrow2/reference/BorrowingFixedPowerPrior-class.md),
[`BorrowingHierarchicalCommensurate-class`](https://genentech.github.io/psborrow2/reference/BorrowingHierarchicalCommensurate-class.md),
[`BorrowingNone-class`](https://genentech.github.io/psborrow2/reference/BorrowingNone-class.md)
