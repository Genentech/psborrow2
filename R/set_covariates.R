# Combine class types
setClassUnion("listOrPrior", c("list","Prior"))

# Covariate class
.covariate_class <- setClass(
   "Covariates",
   slots = c(covariates = "character",
             priors = "listOrPrior"),
   validity = function(object) {
      if (class(object@priors) == "list" &&
          NROW(object@priors) != NROW(object@covariates)) {
         return(paste0("Either specify 1 prior distribution applying to all ",
                       "covariates, or specify a named list with 1 prior per ",
                       "covariate"))
      }
      if (class(object@priors) == "list" &&
          !all(names(object@priors) %in% object@covariates)) {
         return(paste0("If a list is provided to specify priors, one prior per ",
                       "covariate must be provided."))
      }
      if (class(object@priors) == "list" &&
          !all(sapply(object@priors, function(z) is(z, 'Prior')))) {
         return(paste0("If a list is provided to specify priors, all priors ",
                       "must be of class `Prior`"))
      }
      return(TRUE)
   }
)

# Print method
setMethod(
   f = "show",
   signature = "Covariates",
   definition = function(object) {
      cat("Covariate object with priors for variables: ",
          paste0(object@covariates, collapse = ", "))
   }
)

#' Specify column names for adjustment variables in model matrix and prior
#' distributions for the model parameters for these covariates (ie betas)
#'
#' @param covariates character vector naming covariates to be adjusted for
#' @param priors either a single prior distribution applying to all covariates
#' or a named list of prior distributions, one for each covariate
#'
#' @return object of class "Covariates"
#' @export
#'
#' @examples
#' set_covariates(covariates = c('a','b'),
#'                priors = list('a' = normal_prior(0,1),
#'                              'b' = normal_prior(0,2)))
set_covariates <- function(covariates,
                           priors) {

   # Additional errors not captured in class
   if (class(priors) != 'list' &&
       !is(priors, "Prior")) {
      stop("priors argument must be a single object of class `Prior`",
           " or a named list of objects of class `Prior`")
   }

   # Create class
   .covariate_class(covariates = covariates,
                    priors = priors)
}
