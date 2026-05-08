# `BorrowingHierarchicalCommensurate` class

A class for defining details of dynamic borrowing using the hierarchical
Bayesian model with a commensurability parameter. Objects of class
`BorrowingHierarchicalCommensurate` should not be created directly but
by the constructor
[`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md).

## Slots

- `method_name`:

  string. The name of the method.

- `ext_flag_col`:

  character. Name of the external flag column in the matrix.

- `tau_prior`:

  Prior. Prior for the commensurability parameter.

## See also

Other borrowing classes:
[`Borrowing-class`](https://genentech.github.io/psborrow2/reference/Borrowing-class.md),
[`BorrowingFixedPowerPrior-class`](https://genentech.github.io/psborrow2/reference/BorrowingFixedPowerPrior-class.md),
[`BorrowingFull-class`](https://genentech.github.io/psborrow2/reference/BorrowingFull-class.md),
[`BorrowingNone-class`](https://genentech.github.io/psborrow2/reference/BorrowingNone-class.md)
