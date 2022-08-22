#' Make model string of Stan's model block
#'
#' Create the Stan string encompassed by model {}
#'
#' @param analysis_obj `Analysis`. Object of class `Analysis` created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return glue character containing the text for the data block
#'
#' @examples
#' dat <- survival::diabetic
#' dat$ext <- dat$trt == 0 & dat$id > 1000
#' data_mat <- create_data_matrix(
#'   dat,
#'   outcome = c("time", "status"),
#'   trt_flag_col = "trt",
#'   ext_flag_col = "ext"
#' )
#'
#' anls_obj <- psborrow2:::.analysis_obj(
#'   data_matrix = data_mat,
#'   outcome = exp_surv_dist("time", "status"),
#'   borrowing = borrowing_details("Full borrowing", normal_prior(0, 100), "ext"),
#'   treatment = treatment_details("trt", normal_prior(0, 100))
#' )
#'
#' psborrow2:::make_model_string_model(anls_obj)
#'
make_model_string_model <- function(analysis_obj) {
  ## Model string
  model_string <- psborrow2:::h_glue("model {")

  ## Set values shared by all - treatment prior
  object <- analysis_obj@treatment@trt_prior
  beta_trt_prior <- psborrow2:::h_glue(object@stan_code)
  model_string <- psborrow2:::h_glue("
    {{model_string}}
    vector[N] lp;
    vector[N] elp;
    beta_trt ~ {{beta_trt_prior}}")
  rm(object)

  ### Linear predictor
  if (!is.null(analysis_obj@covariates) &&
    analysis_obj@borrowing@method == "BDB") {
    model_string <- psborrow2:::h_glue("
      {{model_string}}
      lp = X * beta + Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
  } else if (is.null(analysis_obj@covariates) &&
    analysis_obj@borrowing@method == "BDB") {
    model_string <- psborrow2:::h_glue("
      {{model_string}}
      lp = Z * alpha + trt * beta_trt;
      elp = exp(lp) ;")
  } else if (!is.null(analysis_obj@covariates) &&
    analysis_obj@borrowing@method != "BDB") {
    model_string <- psborrow2:::h_glue("
      {{model_string}}
      lp = alpha + X * beta + trt * beta_trt ;
      elp = exp(lp) ;")
  } else if (is.null(analysis_obj@covariates) &&
    analysis_obj@borrowing@method != "BDB") {
    model_string <- psborrow2:::h_glue("
      {{model_string}}
      lp = alpha + trt * beta_trt ;
      elp = exp(lp);")
  }

  ### Add priors for relevant parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    for (i in seq_len(NROW(analysis_obj@outcome@param_priors))) {
      name <- names(analysis_obj@outcome@param_priors)[i]
      object <- analysis_obj@outcome@param_priors[[name]]
      value <- psborrow2:::h_glue(object@stan_code)
      prior_str <- psborrow2:::h_glue("{{name}} ~ {{value}} ;")

      model_string <- psborrow2:::h_glue("
        {{model_string}}
        {{prior_str}}")
      rm(object)
    }
  }

  ### Add in tau and alphas if method = BDB
  if (analysis_obj@borrowing@method == "BDB") {
    object <- analysis_obj@borrowing@tau_prior
    tau_prior <- psborrow2:::h_glue(object@stan_code)
    rm(object)

    object <- analysis_obj@borrowing@baseline_prior
    alpha_2_prior <- psborrow2:::h_glue(object@stan_code)
    rm(object)

    model_string <- psborrow2:::h_glue("
      {{model_string}}
      tau ~ {{tau_prior}} ;
      real sigma;
      sigma = 1 / tau;
      alpha[2] ~ {{alpha_2_prior}} ;
      alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;")
  }

  ### Add in alphas if method is not BDB
  if (analysis_obj@borrowing@method != "BDB") {
    object <- analysis_obj@borrowing@baseline_prior
    alpha_prior <- psborrow2:::h_glue(object@stan_code)

    model_string <- psborrow2:::h_glue("
      {{model_string}}
      alpha ~ {{alpha_prior}} ;")
    rm(object)
  }

  ### Add in likelihood function
  model_string <- psborrow2:::h_glue("
    {{model_string}}
    {{analysis_obj@outcome@likelihood_stan_code}}")

  ### Close brackets
  model_string <- psborrow2:::h_glue("
    {{model_string}}
    }")

  ## Return
  return(model_string)
}
