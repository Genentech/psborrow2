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
  ## Model string
  model_string <- h_glue("model {")

  ## Set values shared by all - treatment prior
  beta_trt_prior <- h_glue(analysis_obj@treatment@trt_prior@stan_code, object = analysis_obj@treatment@trt_prior)
  model_string <- h_glue("
    {{model_string}}
    vector[N] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}}; ")

  ### Linear predictor
  has_covariates <- !is.null(analysis_obj@covariates)
  is_bdb <- analysis_obj@borrowing@method == "BDB"

  if (has_covariates && is_bdb) {
    model_string <- h_glue("
      {{model_string}}
      lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
  } else if (!has_covariates && is_bdb) {
    model_string <- h_glue("
      {{model_string}}
      lp = Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
  } else if (has_covariates && !is_bdb) {
    model_string <- h_glue("
      {{model_string}}
      lp = alpha + X * beta + trt * beta_trt ;
      elp = exp(lp) ;")
  } else if (!has_covariates && !is_bdb) {
    model_string <- h_glue("
      {{model_string}}
      lp = alpha + trt * beta_trt ;
      elp = exp(lp);")
  }

  ### Add priors for relevant parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    for (name in names(analysis_obj@outcome@param_priors)) {
      value <- h_glue(analysis_obj@outcome@param_priors[[name]]@stan_code,
        object = analysis_obj@outcome@param_priors[[name]]
      )
      model_string <- h_glue("
        {{model_string}}
        {{name}} ~ {{value}} ;")
    }
  }

  ### Add priors on betas
  if (has_covariates) {
    if (is(analysis_obj@covariates@priors, "Prior")) {
      value <- h_glue(analysis_obj@covariates@priors@stan_code,
        object = analysis_obj@covariates@priors
      )
      for (i in seq_along(analysis_obj@covariates@covariates)) {
        model_string <- h_glue("
          {{model_string}}
          beta[{{i}}] ~ {{value}} ;")
      }
    } else if (is(analysis_obj@covariates@priors, "list")) {
      for (i in seq_along(analysis_obj@covariates@priors)) {
        value <- h_glue(analysis_obj@covariates@priors[[i]]@stan_code,
          object = analysis_obj@covariates@priors[[i]]
        )
        model_string <- h_glue("
          {{model_string}}
          beta[{{i}}] ~ {{value}} ;")
      }
    }
  }

  ### Add in tau and alphas if method = BDB
  if (analysis_obj@borrowing@method == "BDB") {
    tau_prior <- h_glue(analysis_obj@borrowing@tau_prior@stan_code, object = analysis_obj@borrowing@tau_prior)

    alpha_2_prior <- h_glue(
      analysis_obj@outcome@baseline_prior@stan_code,
      object = analysis_obj@outcome@baseline_prior
    )

    model_string <- h_glue("
      {{model_string}}
      tau ~ {{tau_prior}} ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ {{alpha_2_prior}} ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;")
  }

  ### Add in alphas if method is not BDB
  if (analysis_obj@borrowing@method != "BDB") {
    alpha_prior <- h_glue(
      analysis_obj@outcome@baseline_prior@stan_code,
      object = analysis_obj@outcome@baseline_prior
    )

    model_string <- h_glue("
      {{model_string}}
      alpha ~ {{alpha_prior}} ;")
  }

  ### Add in likelihood function
  model_string <- h_glue("
    {{model_string}}
    {{analysis_obj@outcome@likelihood_stan_code}}")

  ### Close brackets
  model_string <- h_glue("
    {{model_string}}
    }")

  ## Return
  return(model_string)
}
