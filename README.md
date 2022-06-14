# psborrow2

## Overview

The `psborrow2` package exists to provide an efficient framework for users to
conduct Bayesian Dynamic Borrowing (BDB) analyses (for more information on BDB,
see Ibrahim et al., 2000; Hobbs et al., 2012; Schmidli et al., 2014;
Lewis, et al., 2019). 

`psborrow2` has two main goals: First, facilitate simulation studies that
compare different borrowing parameters (e.g. full borrowing, no borrowing, BDB)
and other trial and BDB characteristics (e.g. sample size, covariates)
in a unified way; and second, provide a user-friendly interface for applying
BDB on the study results that handles the MCMC sampling on behalf of the user.

`psborrow2` is the successor of
[`psborrow`](https://github.com/Genentech/psborrow). Major updates to
`psborrow2` include:

* New MCMC software (STAN)
* New user interface
* Additional functionality

## Installation
`psborrow2` is currently under development and should therefore be used with
caution. You can install the development version via:

```r
remotes::install_github("Genentech/psborrow2")
```

Please note that this package requires [`cmdstanr`](https://mc-stan.org/cmdstanr/)
to be installed.
