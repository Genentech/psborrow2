#' Make model string of Stan's parameters model block
#'
#' Create the Stan string encompassed by parameters {}
#'
#' @param analysis_obj `Analysis`. Object of class `Analysis` created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return glue character containing the text for the functions block
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
#' psborrow2:::make_model_string_parameters(anls_obj)
#'
make_model_string_parameters <- function(analysis_obj) {
  ## Parameters string
  parameters_string <- psborrow2:::h_glue("
    parameters {
    real beta_trt;}")

  ### Set tau and alpha[2] for BDB
  if (analysis_obj@borrowing@method == "BDB") {
    parameters_string <- psborrow2:::h_glue("
      {{parameters_string}}
      real<lower=0> tau;
      vector[2] alpha;")
  }

  ### Set alpha for non-BDB
  if (analysis_obj@borrowing@method != "BDB") {
    parameters_string <- psborrow2:::h_glue("
      {{parameters_string}}
      real alpha;")
  }

  ### Add outcome specific parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    for (i in seq_len(NROW(analysis_obj@outcome@param_priors))) {
      parameters_string <- psborrow2:::h_glue("
        {{parameters_string}}
        real {{names(analysis_obj@outcome@param_priors[i])}} ;")
    }
  }

  ### Add in vector of coefficients if covariates are provided
  if (!is.null(analysis_obj@covariates)) {
    parameters_string <- psborrow2:::h_glue("
      {{parameters_string}}
      vector[K] beta ;")
  }

  ### Close block
  parameters_string <- psborrow2:::h_glue("{{parameters_string}} }")

  # Return
  return(parameters_string)
}
