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
    name_betas = stats::setNames(h_glue("beta[{{seq_along(covariates)}}]"), covariates)
  )
}
