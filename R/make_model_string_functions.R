#' Make model string of Stan's functions model bloc
#'
#' Create the Stan string encompassed by functions `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return `glue` `character` containing the text for the functions block.
#'
#' @examples
#' anls_obj <- psborrow2:::.analysis_obj(
#'   data_matrix = example_matrix,
#'   outcome = exp_surv_dist("time", "cnsr"),
#'   borrowing = borrowing_details(
#'     "Full borrowing",
#'     normal_prior(0, 100),
#'     "ext"
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 100))
#' )
#'
#' psborrow2:::make_model_string_functions(anls_obj)
#'
make_model_string_functions <- function(analysis_obj) {
  ## Bring in analysis_obj functions
  functions_string <- h_glue("
    functions {
      {{analysis_obj@outcome@function_stan_code}}
    }")
  return(functions_string)
}
