#' Make model string of Stan's transformed parameters model block
#'
#' Create the Stan string encompassed by transformed parameters `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `.analysis_obj()`.
#'
#' @return `glue` `character` containing the Stan code for the functions block.
#'
#' @examples
#' anls_obj <- .analysis_obj(
#'   data_matrix = example_matrix,
#'   outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
#'   borrowing = borrowing_full("ext"),
#'   treatment = treatment_details("trt", prior_normal(0, 100))
#' )
#' 
#' make_model_string_transf_param(anls_obj)
#' @noRd
make_model_string_transf_param <- function(analysis_obj) {
  transformed_parameters_string <- if (is(analysis_obj@outcome, "TimeToEvent")) {
    "real HR_trt = exp(beta_trt);"
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    "real OR_trt = exp(beta_trt);"
  } else {
    ""
  }

  h_glue("transformed parameters {
    {{transformed_parameters_string}}
  }")
}
