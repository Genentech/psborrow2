#' `CovariateClass` Class
#'
#' A class for defining covariate adjustment details. Objects of class
#' `CovariateClass` should not be created directly but by the constructor
#' [add_covariates()].
#'
#' @slot n_covs integer. Number of covariates to adjust for
#' @family covariate classes
#' @seealso Prior constructor functions: [add_covariates()], [no_covariates()]
setClass(
  "CovariateClass",
  slots = c(
    n_covs = "integer"
  ),
  contains = "VIRTUAL"
)
