# `SimVarCont` class

A constructor for making objects of class `SimVarCont`. Objects of class
`SimVarCont` are used to hold mean values of of continuous variables
specified in a simulation study.

## Slots

- `mu_internal`:

  numeric. Mean covariate value for the internal arms.

- `mu_external`:

  numeric. Mean covariate value for the external arm.

- `printval_int`:

  numeric. Value to print to summarize internal arms.

- `printval_ext`:

  numeric. Value to print to summarize external arm.

- `type_string`:

  character. 'continuous'

## See also

Other simvar classes:
[`SimVarBin-class`](https://genentech.github.io/psborrow2/reference/SimVarBin-class.md)
