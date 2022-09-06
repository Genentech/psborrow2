#' Make model string of Stan's transformed parameters model block
#'
#' Create the Stan string encompassed by transformed parameters `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return `glue` `character` containing the Stan code for the functions block.
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
#' psborrow2:::make_model_string_transf_param(anls_obj)
#'
make_model_string_transf_param <- function(analysis_obj) {
  ## Transformed parameters string
  transformed_parameters_string <- h_glue("
    transformed parameters {")

  ## Exponentiate effect estimate
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    transformed_parameters_string <- h_glue("
      {{transformed_parameters_string}}
      real HR_trt = exp(beta_trt);")
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    transformed_parameters_string <- h_glue("
      {{transformed_parameters_string}}
      real OR_trt = exp(beta_trt);")
  }

  ### Close block
  transformed_parameters_string <- h_glue("{{transformed_parameters_string}} }")

  # Return
  return(transformed_parameters_string)
}
