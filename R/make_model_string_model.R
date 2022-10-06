#' Make model string of Stan's model block
#'
#' Create the Stan string encompassed by model `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return `glue` `character` containing the Stan code for the data block.
#'
#' @examples
#' anls_obj <- psborrow2:::.analysis_obj(
#'   data_matrix = example_matrix,
#'   outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100)),
#'   borrowing = borrowing_details(
#'     "Full borrowing",
#'     "ext"
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 100))
#' )
#'
#' psborrow2:::make_model_string_model(anls_obj)
#'
make_model_string_model <- function(analysis_obj) {
  # treatment prior
  beta_trt_prior <- get_prior_string(analysis_obj@treatment@trt_prior)

  ### Linear predictor
  has_covariates <- !is.null(analysis_obj@covariates)
  is_bdb <- analysis_obj@borrowing@method == "BDB"

  if (has_covariates && is_bdb) {
    linear_predictor <- h_glue("
      lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;
    ")
  } else if (!has_covariates && is_bdb) {
    linear_predictor <- h_glue("
      lp = Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
  } else if (has_covariates && !is_bdb) {
    linear_predictor <- h_glue("
      lp = alpha + X * beta + trt * beta_trt ;
      elp = exp(lp) ;")
  } else if (!has_covariates && !is_bdb) {
    linear_predictor <- h_glue("
      lp = alpha + trt * beta_trt ;
      elp = exp(lp);")
  } else {
    stop("No linear predictor defined.")
  }

  ### Add priors for relevant parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    names <- names(analysis_obj@outcome@param_priors)
    values <- get_prior_string(analysis_obj@outcome@param_priors)
    outcome_prior <- h_glue("{{names}} ~ {{values}} ;", collapse = TRUE)
  } else {
    outcome_prior <- ""
  }

  ### Add priors on betas
  if (has_covariates) {
    i <- seq_along(analysis_obj@covariates@covariates)
    value <- get_prior_string(analysis_obj@covariates@priors)
    index <- if (test_named(value)) get_vars(analysis_obj@covariates) else rep(1, length(i))
    covariate_prior <- h_glue("beta[{{i}}] ~ {{value[index]}} ;", collapse = TRUE)
  } else {
    covariate_prior <- ""
  }

  ### Add in tau and alphas if method = BDB
  if (analysis_obj@borrowing@method == "BDB") {
    tau_prior <- get_prior_string(analysis_obj@borrowing@tau_prior)
    alpha_2_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)

    borrowing_string <- h_glue("
      tau ~ {{tau_prior}} ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ {{alpha_2_prior}} ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;")
  } else if (analysis_obj@borrowing@method != "BDB") {
    alpha_prior <- get_prior_string(analysis_obj@outcome@baseline_prior)
    borrowing_string <- h_glue("alpha ~ {{alpha_prior}} ;")
  } else {
    borrowing_string <- ""
  }

  ### Add in likelihood function
  likelihood_string <- h_glue("{{analysis_obj@outcome@likelihood_stan_code}}")

  model_string <- h_glue("
  model {
    vector[N] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}};
    {{linear_predictor}}
    {{outcome_prior}}
    {{covariate_prior}}
    {{borrowing_string}}
    {{likelihood_string}}
  }")

  return(model_string)
}
