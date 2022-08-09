#' @include prior_class.R

# Combine class types
setClassUnion("listOrPrior", c("list", "Prior"))

# Covariate class
.covariate_class <- setClass(
   "Covariates",
   slots = c(
      covariates = "character",
      priors = "listOrPrior"
   ),
   validity = function(object) {
      if (is(object@priors, "list") &&
          NROW(object@priors) != NROW(object@covariates)) {
         return(paste0(
            "Either specify 1 prior distribution for all ",
            "covariates, or specify a named list with 1 prior per ",
            "covariate"
         ))
      }

      if (is(object@priors, "list") &&
          !all(sapply(object@priors, is, "Prior"))) {
         return(paste0(
            "If a list is provided to specify priors, all priors ",
            "must be of class `Prior`"
         ))
      }

      if (is(object@priors, "list") &&
          !all(names(object@priors) %in% object@covariates)) {
         return(paste0(
            "If a list is provided to specify priors, one prior per ",
            "covariate must be provided."
         ))
      }

      return(TRUE)
   }
)

# Print method
setMethod(
   f = "show",
   signature = "Covariates",
   definition = function(object) {
      cat(
         "Covariate object with priors for variables: ",
         paste0(object@covariates, collapse = ", ")
      )
   }
)
