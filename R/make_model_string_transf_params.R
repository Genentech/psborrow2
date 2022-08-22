#' Make model string of Stan's transformed parameters model block
#'
#' Create the Stan string encompassed by transformed parameters {}
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
#' psborrow2:::make_model_string_transf_param(anls_obj)
#'
make_model_string_transf_param <- function(analysis_obj) {
  ## Transformed parameters string
  transformed_parameters_string <- psborrow2:::h_glue("
    transformed parameters {")

  ## Exponentiate effect estimate
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    transformed_parameters_string <- psborrow2:::h_glue("
      {{transformed_parameters_string}}
      real HR_trt = exp(beta_trt);")
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    transformed_parameters_string <- psborrow2:::h_glue("
      {{transformed_parameters_string}}
      real OR_trt = exp(beta_trt);")
  }

  ### Close block
  transformed_parameters_string <- psborrow2:::h_glue("{{transformed_parameters_string}} }")

  # Return
  return(transformed_parameters_string)
}
