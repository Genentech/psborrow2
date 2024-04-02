#' Make model string of Stan's functions model bloc
#'
#' Create the Stan string encompassed by functions `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `.analysis_obj()`.
#'
#' @return `glue` `character` containing the text for the functions block.
#'
#' @examples
#' anls_obj <- .analysis_obj(
#'   data_matrix = example_matrix,
#'   outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
#'   borrowing = borrowing_full("ext"),
#'   treatment = treatment_details("trt", prior_normal(0, 100))
#' )
#'
#' make_model_string_functions(anls_obj)
#' @noRd
make_model_string_functions <- function(analysis_obj) {
  ## Bring in analysis_obj functions
  h_glue("
    functions {
      {{analysis_obj@outcome@function_stan_code}}
    }")
}
