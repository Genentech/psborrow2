# Hierarchical commensurate borrowing

Hierarchical commensurate borrowing

## Usage

``` r
borrowing_hierarchical_commensurate(ext_flag_col, tau_prior)
```

## Arguments

- ext_flag_col:

  character. Name of the external flag column in the matrix.

- tau_prior:

  Prior. Prior for the commensurability parameter.

## Value

Object of class
[`BorrowingHierarchicalCommensurate`](https://genentech.github.io/psborrow2/reference/BorrowingHierarchicalCommensurate-class.md).

## Details

### Method

In Bayesian dynamic borrowing using the hierarchical commensurate prior
approach, external control information is borrowed to the extent that
the outcomes (i.e., log hazard rates or log odds) are similar between
external and internal control populations. See Viele 2014
[doi:10.1002/pst.1589](https://doi.org/10.1002/pst.1589) and Hobbs 2011
[doi:10.1111/j.1541-0420.2011.01564.x](https://doi.org/10.1111/j.1541-0420.2011.01564.x)
for details.

### External Control

The `ext_flag_col` argument refers to the column in the data matrix that
contains the flag indicating a patient is from the external control
cohort.

### Tau Prior

The `tau_prior` argument specifies the hyperprior on the precision
parameter commonly referred to as the commensurability parameter. See
Viele 2014 [doi:10.1002/pst.1589](https://doi.org/10.1002/pst.1589) for
more details. This hyperprior determines (along with the comparability
of the outcomes between internal and external controls) how much
borrowing of the external control group will be performed. Example
hyperpriors include largely uninformative inverse gamma distributions
\[e.g., `prior_gamma(alpha = .001, beta = .001)`\] as well as more
informative distributions \[e.g.,
`prior_gamma(alpha = 1, beta = .001`)\], though any distribution \\x \in
(0, \infty)\\ can be used. Distributions with more density at higher
values of \\x\\ (i.e., higher precision) will lead to more borrowing.

## References

Viele, K., Berry, S., Neuenschwander, B., Amzal, B., Chen, F., Enas, N.,
Hobbs, B., Ibrahim, J.G., Kinnersley, N., Lindborg, S., Micallef, S.,
Roychoudhury, S. and Thompson, L. (2014), Use of historical control data
for assessing treatment effects in clinical trials. **Pharmaceut.
Statist., 13: 41–54**.
[doi:10.1002/pst.1589](https://doi.org/10.1002/pst.1589)

Hobbes, B.P., Carlin, B.P., Mandrekar, S.J. and Sargent, D.J. (2011),
Hierarchical commensurate and power prior models for adaptive
incorporation of historical information in clinical trials.
**Biometrics, 67: 1047–1056**.
[doi:10.1111/j.1541-0420.2011.01564.x](https://doi.org/10.1111/j.1541-0420.2011.01564.x)

## Examples

``` r
db <- borrowing_hierarchical_commensurate(
  ext_flag_col = "ext",
  tau_prior = prior_gamma(0.0001, 0.0001)
)
```
