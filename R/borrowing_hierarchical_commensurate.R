#' `BorrowingHierarchicalCommensurate` class
#'
#' A class for defining details of dynamic borrowing
#' using the hierarchical Bayesian model with a commensurability
#' parameter. Objects of class `BorrowingHierarchicalCommensurate`
#' should not be created directly but by the constructor
#' [borrowing_hierarchical_commensurate()].
#'
#' @slot data_stan_code string. Code to include in the Stan data program block.
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot tau_prior Prior. Prior for the commensurability parameter.
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_hierarchical_commensurate <- setClass(
  "BorrowingHierarchicalCommensurate",
  slots = c(
    tau_prior = "Prior"
  ),
  prototype = list(
    data_stan_code = "matrix[N,2] Z;",
    method_name = "Bayesian dynamic borrowing with the hierarchical commensurate prior"
  ),
  contains = "Borrowing",
  validity = function(object) {
    if (parse_constraint(object@tau_prior)["lower"] < 0) {
      return("tau distribution must be bounded >=0")
    }
    return(TRUE)
  }
)

#' Hierarchical commensurate borrowing
#'
#' @param tau_prior Prior. Prior for the commensurability parameter.
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#'
#' @details
#'
#' ## Method
#'
#' In Bayesian dynamic borrowing using the hierarchical commensurate prior approach,
#' external control information is borrowed to the extent that the
#' outcomes (i.e., log hazard rates or log odds) are similar between
#' external and internal control populations. See Viele 2014
#' \doi{10.1002/pst.1589} and Hobbs 2011
#' \doi{10.1111/j.1541-0420.2011.01564.x} for details.
#'
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#'
#' ## Tau Prior
#'
#' The `tau_prior` argument specifies the hyperprior on the precision parameter
#' commonly referred to as the commensurability parameter.
#' See Viele 2014 \doi{10.1002/pst.1589} for more
#' details.
#' This hyperprior determines (along with the comparability of the outcomes
#' between internal and external controls) how much borrowing of the external
#' control group will be performed.
#' Example hyperpriors include largely uninformative inverse gamma distributions
#' \[e.g., `prior_gamma(alpha = .001, beta = .001)`\] as well as more
#' informative distributions \[e.g., `prior_gamma(alpha = 1, beta = .001`)\],
#' though any distribution \eqn{x \in (0, \infty)} can be used. Distributions
#' with more density at higher values of \eqn{x} (i.e., higher precision)
#' will lead to more borrowing.
#'
#' @references
#'
#' Viele, K., Berry, S., Neuenschwander, B., Amzal, B., Chen, F.,
#'  Enas, N., Hobbs, B., Ibrahim, J.G., Kinnersley, N., Lindborg, S., Micallef,
#'   S., Roychoudhury, S. and Thompson, L. (2014),
#'   Use of historical control data for assessing treatment effects in clinical trials.
#'   __Pharmaceut. Statist., 13: 41--54__. \doi{10.1002/pst.1589}
#'
#' Hobbes, B.P., Carlin, B.P., Mandrekar, S.J. and Sargent, D.J. (2011),
#'  Hierarchical commensurate and power prior models for adaptive incorporation of historical information in clinical trials.
#'  __Biometrics, 67: 1047--1056__. \doi{10.1111/j.1541-0420.2011.01564.x}
#'
#' @return Object of class [`BorrowingHierarchicalCommensurate`][BorrowingHierarchicalCommensurate-class].
#' @export
#' @examples
#' db <- borrowing_hierarchical_commensurate(
#'   ext_flag_col = "ext",
#'   tau_prior = prior_gamma(0.0001, 0.0001)
#' )
borrowing_hierarchical_commensurate <- function(ext_flag_col, tau_prior) {
  assert_class(tau_prior, "Prior")
  assert_string(ext_flag_col)
  .borrowing_hierarchical_commensurate(ext_flag_col = ext_flag_col, tau_prior = tau_prior)
}

# show ----
setMethod(
  f = "show",
  signature = "BorrowingHierarchicalCommensurate",
  definition = function(object) {
    callNextMethod()
    cat("Commensurability parameter prior:\n")
    show(object@tau_prior)
  }
)

# trim cols ----
#' @rdname trim_cols
#' @include generics.R
setMethod(
  f = "trim_cols",
  signature = "BorrowingHierarchicalCommensurate",
  definition = function(borrowing_object, analysis_object) {
    return(get_vars(analysis_object))
  }
)

#' @rdname create_alpha_string
#' @include generics.R
setMethod(
  f = "create_alpha_string",
  signature = "BorrowingHierarchicalCommensurate",
  definition = function(borrowing_object, outcome_object) {
    return(setNames(c("alpha[1]", "alpha[2]"), paste0(outcome_object@alpha_type, c(", internal", ", external"))))
  }
)

#' @rdname create_tau_string
#' @include generics.R
setMethod(
  f = "create_tau_string",
  signature = "BorrowingHierarchicalCommensurate",
  definition = function(borrowing_object) {
    return(c("commensurability parameter" = "tau"))
  }
)
