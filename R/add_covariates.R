# class union----
setClassUnion("listOrPrior", c("list", "Prior"))

#' `Covariates` Class
#'
#' A class for defining covariate details. Objects of class
#' `Covariate` should not be created directly but by the constructor
#' [add_covariates()].
#'
#' @slot covariates character. Names of columns in the data matrix containing
#' covariates to be adjusted for in the outcome model. Note: the
#' external and treatment flags should not go here.
#' @slot priors. Either a single object of class `Prior` specifying the prior
#' distribution to apply to all covariates or a named list of
#' distributions of class `Prior`, one for each covariate
#' @slot name_betas. Names for the beta parameters in the STAN model.
#' @slot n_covs. Number of covariates to adjust for.
#' @family covariate classes
#' @include prior_class.R covariate_class.R
.covariate_class <- setClass(
  "Covariates",
  slots = c(
    covariates = "character",
    priors = "listOrPrior",
    name_betas = "character"
  ),
  validity = function(object) {
    if (is(object@priors, "list")) {
      if (NROW(object@priors) != NROW(object@covariates)) {
        return(paste0(
          "Either specify 1 prior distribution for all ",
          "covariates, or specify a named list with 1 prior per ",
          "covariate"
        ))
      }

      if (!all(vapply(object@priors, is, logical(1L), class2 = "Prior"))) {
        return(paste0(
          "If a list is provided to specify priors, all priors ",
          "must be of class `Prior`"
        ))
      }

      if (!all(names(object@priors) %in% object@covariates)) {
        return(paste0(
          "If a list is provided to specify priors, one prior per ",
          "covariate must be provided."
        ))
      }
    }

    return(TRUE)
  },
  contains = "CovariateClass"
)

# show ----
setMethod(
  f = "show",
  signature = "Covariates",
  definition = function(object) {
    cat("Covariate object with priors for variables:\n")
    cat(toString(object@covariates), "\n\n")
    if (is(object@priors, "Prior")) {
      cat("Prior for all covariate coefficients:\n")
      show(object@priors)
    } else if (is(object@priors, "list")) {
      for (i in object@covariates) {
        cat("Prior for ", i, ":\n", sep = "")
        show(object@priors[[i]])
        cat("\n")
      }
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "Covariates",
  definition = function(object) {
    c(object@covariates)
  }
)


#' Add Covariates for Model Adjustment
#'
#' Specify column names for adjustment variables in model matrix and prior
#' distributions for the model parameters for these covariates (i.e., betas)
#'
#' @param covariates character. Names of columns in the data matrix containing
#' covariates to be adjusted for in the outcome model. Note: the
#' external and treatment flags should not go here.
#' @param priors Either a single object of class `Prior` specifying the prior
#' distribution to apply to all covariates or a named list of
#' distributions of class `Prior`, one for each covariate
#'
#' @return Object of class [`Covariates`][Covariates-class].
#' @export
#' @include covariate_class.R
#'
#' @examples
#' add_covariates(
#'   covariates = c("a", "b"),
#'   priors = list(
#'     "a" = prior_normal(0, 1),
#'     "b" = prior_normal(0, 2)
#'   )
#' )
add_covariates <- function(covariates,
                           priors) {
  if (!is(priors, "listOrPrior")) {
    stop(
      "priors argument must be a single object of class `Prior`",
      " or a named list of objects of class `Prior`"
    )
  }
  assert_character(covariates)

  .covariate_class(
    covariates = covariates,
    priors = priors,
    name_betas = stats::setNames(h_glue("beta[{{seq_along(covariates)}}]"), covariates),
    n_covs = NROW(covariates)
  )
}
