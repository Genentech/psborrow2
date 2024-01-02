# class union----
setClassUnion("listOrPrior", c("list", "Prior"))

#' `Covariate` Class
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
#' @include prior_class.R
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
  }
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
