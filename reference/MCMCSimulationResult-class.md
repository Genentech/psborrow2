# `MCMCSimulationResult` Class

A class for defining Simulation study results. Objects of class
`MCMCSimulationResult` should not be created directly but by
[`mcmc_sample()`](https://genentech.github.io/psborrow2/reference/mcmc_sample.md).

## Slots

- `results`:

  `data.frame`. The results of the simulation study summarized in a
  `data.frame`

- `cmd_stan_models`:

  list. List of lists of `CmdStanmodels` corresponding to the different
  parameters in `Simulation@guide` and different datasets in
  `Simulation@data_matrix_list`.
