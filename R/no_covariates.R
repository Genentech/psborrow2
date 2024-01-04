#' `NoCovariates` Class
#'
#' A class for no covariates. Objects of class
#' `NoCovariate` should not be created directly but by the constructor
#' [no_covariates()].
#'
#' @slot n_covs. Number of covariates to adjust for.
#' @family covariate classes
#' @include prior_class.R covariate_class.R
.no_covariate_class <- setClass(
  "NoCovariates",
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

#' Placeholder for No Covariates for Model Adjustment
#'#'
#' @return Object of class [`NoCovariates`][NoCovariates-class].
#' @include covariate_class.R
no_covariates <- function() {

  .no_covariate_class()

}
