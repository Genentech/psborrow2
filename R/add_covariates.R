#' Specify column names for adjustment variables in model matrix and prior
#' distributions for the model parameters for these covariates (i.e., betas)
#'
#' @param covariates character vector naming covariates to be adjusted for in
#' the outcome model
#' @param priors either a single prior distribution applying to all covariates
#' or a named list of prior distributions, one for each covariate
#'
#' @return object of class "Covariates"
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' add_covariates(
#'    covariates = c("a", "b"),
#'    priors = list(
#'       "a" = normal_prior(0, 1),
#'       "b" = normal_prior(0, 2)
#'    )
#' )
#' @include covariate_class.R
add_covariates <- function(covariates,
                           priors) {

   # Better messaging for errors that are already considered in class
   if (!is(priors, "listOrPrior")) {
      stop(
         "priors argument must be a single object of class `Prior`",
         " or a named list of objects of class `Prior`"
      )
   }

   assert_character(covariates)

   # Create class
   .covariate_class(
      covariates = covariates,
      priors = priors
   )
}
