#' Get prior string for all covariates
#'
#' @param covariates `Covariates` object
#' @include generics.R covariate_class.R
get_prior_string_covariates <- function(covariates) {
  assert_class(covariates, "Covariates")
  i <- seq_along(covariates@covariates)
  value <- get_prior_string(covariates@priors)
  index <- if (test_named(value)) get_vars(covariates) else rep(1, length(i))
  covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
  return(covariate_prior)
}
