# `SimVarBin` class

A constructor for making objects of class `SimVarBin`. Objects of class
`SimVarBin` are used to hold proportions of binary variables specified
in a simulation study.

## Slots

- `prob_internal`:

  numeric. Proportion for the internal arms.

- `prob_external`:

  numeric. Proportion for the external arm.

- `mu_internal_before_bin`:

  numeric. Mean value of covariate before binarization for the internal
  arms.

- `mu_external_before_bin`:

  numeric. Mean value of covariate before binarization for the external
  arm.

- `printval_int`:

  numeric. Value to print to summarize internal arms.

- `printval_ext`:

  numeric. Value to print to summarize external arm.

- `type_string`:

  character. 'binary'

## See also

Other simvar classes:
[`SimVarCont-class`](https://genentech.github.io/psborrow2/reference/SimVarCont-class.md)
