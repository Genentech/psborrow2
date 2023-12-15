#' `BorrowingHierarchicalCommensurate` class
#' 
#' A class for defining details of dynamic borrowing 
#' using the hierarchical Bayesian model with a commensurability 
#' parameter. Objects of class `BorrowingHierarchicalCommensurate`
#' should not be created directly but by the constructor
#' [borrowing_hierarchical_commensurate()].
#' 
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot tau_prior Prior. Prior for the commensurability parameter.
#' @slot data_stan_code string. Stan code that will be interpolated into the model.
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_hierarchical_commensurate <- setClass(
   "BorrowingHierarchicalCommensurate",
   slots = c(
      ext_flag_col = "character",
      tau_prior = "Prior"
   ),
   prototype = list(
      data_stan_code = "" #@TODO update STAN code here
   ),
   contains = "Borrowing"
)

#' Hierarchical commensurate borrowing
#' 
#' @param tau_prior Prior. Prior for the commensurability parameter.
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#' 
#' @details
#' 
#' ## Method
#' In Bayesian dynamic borrowing using the hierarchical commensurate prior approach, 
#' external control information is borrowed to the extent that the
#' outcomes (i.e., log hazard rates or log odds) are similar between
#' external and internal control populations. See
#' \href{https://doi.org/10.1002/pst.1589}{Viele et. al. 2014} and
#' \href{https://doi.org/10.1111/j.1541-0420.2011.01564.x}{Hobbs et. al. 2011}
#' for details.
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
#' See \href{https://doi.org/10.1002/pst.1589}{Viele et. al. 2014} for more
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
#' @include borrowing_class.R
#' 
#' @examples
#' db <- borrowing_hierarchical_commensurate(
#'    ext_flag_col = "ext",
#'    tau_prior = gamma_prior(0.0001, 0.0001)
#' )
borrowing_hierarchical_commensurate <- function(ext_flag_col, tau_prior) {
   .borrowing_hierarchical_commensurate(ext_flag_col = ext_flag_col, tau_prior = tau_prior)
}

# show ----
setMethod(
  f = "show",
  signature = "BorrowingHierarchicalCommensurate",
  definition = function(object) {
    cat("Borrowing object using the hierarchical commensurate prior approach\n\n")
    cat("External control flag:", object@ext_flag_col, "\n\n")
    cat("Commensurability parameter prior:\n")
    show(object@tau_prior)
   }
)