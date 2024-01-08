#' `NoCovariates` Class
#'
#' A class for no covariates. Objects of class
#' `NoCovariate` should not be created directly but by the constructor
#' [no_covariates()].
#'
#' @slot name_betas. Names for the beta parameters in the STAN model (NULL).
#' @slot n_covs. Number of covariates to adjust for.
#' @family covariate classes
#' @include prior_class.R covariate_class.R
.no_covariate_class <- setClass(
  "NoCovariates",
   slots = c(
      name_betas = "NULL"
   ),
   prototype = prototype(
      name_betas = NULL
   ),
  contains = "CovariateClass"
)

# show ----
setMethod(
  f = "show",
  signature = "NoCovariates",
  definition = function(object) {
      cat("NoCovariates object\n\n")
  }
)

#' @rdname get_vars
#' @include generics.R
#' @usage \S4method{get_vars}{NoCovariates}(object)
setMethod(
  f = "get_vars",
  signature = "NoCovariates",
  definition = function(object) {
    NULL
  }
)


#' Placeholder for No Covariates for Model Adjustment
#'#'
#' @return Object of class [`NoCovariates`][NoCovariates-class].
#' @include covariate_class.R
no_covariates <- function() {

  .no_covariate_class()

}
