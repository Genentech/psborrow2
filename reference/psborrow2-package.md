# psborrow2: Bayesian Dynamic Borrowing Analysis and Simulation

Bayesian dynamic borrowing is an approach to incorporating external data
to supplement a randomized, controlled trial analysis in which external
data are incorporated in a dynamic way (e.g., based on similarity of
outcomes); see Viele 2013
[doi:10.1002/pst.1589](https://doi.org/10.1002/pst.1589) for an
overview. This package implements the hierarchical commensurate prior
approach to dynamic borrowing as described in Hobbes 2011
[doi:10.1111/j.1541-0420.2011.01564.x](https://doi.org/10.1111/j.1541-0420.2011.01564.x)
. There are three main functionalities. First, 'psborrow2' provides a
user-friendly interface for applying dynamic borrowing on the study
results handles the Markov Chain Monte Carlo sampling on behalf of the
user. Second, 'psborrow2' provides a simulation framework to compare
different borrowing parameters (e.g. full borrowing, no borrowing,
dynamic borrowing) and other trial and borrowing characteristics (e.g.
sample size, covariates) in a unified way. Third, 'psborrow2' provides a
set of functions to generate data for simulation studies, and also
allows the user to specify their own data generation process. This
package is designed to use the sampling functions from 'cmdstanr' which
can be installed from <https://stan-dev.r-universe.dev>.

## See also

Useful links:

- <https://github.com/Genentech/psborrow2>

- <https://genentech.github.io/psborrow2/index.html>

- Report bugs at <https://github.com/Genentech/psborrow2/issues>

## Author

**Maintainer**: Matt Secrest <secrestm@gene.com>
([ORCID](https://orcid.org/0000-0002-0939-4902))

Authors:

- Isaac Gravestock <isaac.gravestock@roche.com>

Other contributors:

- Craig Gower-Page <craig.gower-page@roche.com> \[contributor\]

- Manoj Khanal <khanal_manoj@lilly.com> \[contributor\]

- Mingyang Shan <mingyang.shan@lilly.com> \[contributor\]

- Kexin Jin <kexin.jin@bms.com> \[contributor\]

- Zhi Yang <zhi.yang@bms.com> \[contributor\]

- Genentech, Inc. \[copyright holder, funder\]
