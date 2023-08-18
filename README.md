# psborrow2 <img src="./man/figures/hex.png" align="right" width="120"/>

<!-- badges: start -->

[![Version](https://img.shields.io/static/v1.svg?label=github.com/genentech&message=v.0.0.2.0&color=DC0073)](https://github.com/Genentech/psborrow2)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

## Overview

`psborrow2` is an R package that for conducting Bayesian Dynamic Borrowing (BDB)
analyses and simulation studies.[^1] [^2]
`psborrow2` has two main objectives:

1. **Facilitate BDB analyses**. `psborrow2` has a user-friendly interface for
   conducting BDB analyses that handles the computationally-difficult MCMC sampling
   on behalf of the user.

2. **Facilitate simulation studies of BDB**. `psborrow2` includes a
   framework to compare different trial and BDB characteristics in a unified way
   in simulation studies to inform trial design.

## Background

`psborrow2` is the successor to
[`psborrow`](https://github.com/Genentech/psborrow). [`psborrow`](https://github.com/Genentech/psborrow)
is still freely available on [`CRAN`](https://cran.r-project.org/package=psborrow) with the
same validated functionality; however, the package is not actively developed.
Major updates in `psborrow2` include:

- New, more flexible user interface
- New MCMC software (STAN)
- Expanded functionality

The name `psborrow` combines propensity scoring (`ps`) and Bayesian dynamic
`borrow`ing. As the name implies, this package can be used to combine dynamic
borrowing and propensity-score adjustment/weighting methods.

## Installation

You can install the latest version of `psborrow2` with:

```r
remotes::install_github("Genentech/psborrow2")
```

Please note that this package requires [`cmdstanr`](https://mc-stan.org/cmdstanr/).

## Vignettes

To learn how to use the `psborrow2` R package, refer to the vignettes on the 
package website:

[https://genentech.github.io/psborrow2/main/articles/index.html](https://genentech.github.io/psborrow2/main/articles/index.html)

## Bibliography

[^1]:
    Lewis CJ, Sarkar S, Zhu J, Carlin BP. Borrowing from historical control data
    in cancer drug development: a cautionary tale and practical guidelines.
    Statistics in biopharmaceutical research. 2019 Jan 2;11(1):67-78. [https://doi.org/10.1080/19466315.2018.1497533](https://doi.org/10.1080/19466315.2018.1497533)

[^2]:
    Viele K, Berry S, Neuenschwander B, Amzal B, Chen F, Enas N, Hobbs B,
    Ibrahim JG, Kinnersley N, Lindborg S, Micallef S. Use of historical control
    data for assessing treatment effects in clinical trials. Pharmaceutical
    statistics. 2014 Jan;13(1):41-54. [https://doi.org/10.1002/pst.1589](https://doi.org/10.1002/pst.1589)
