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
#'   borrowing = borrowing_details(
#'     "Full borrowing",
#'     normal_prior(0, 100),
#'     "extTRUE"
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 100))
#' )
#'
#' psborrow2:::make_model_string_functions(anls_obj)
#'
make_model_string_functions <- function(analysis_obj) {
  ## Functions string
  functions_string <- h_glue("functions {")

  ## Bring in analysis_obj functions
  functions_string <- h_glue("
    {{functions_string}}
    {{analysis_obj@outcome@function_stan_code}}")

  ## Close block
  functions_string <- h_glue("
    {{functions_string}}
    }")

  # Return
  return(functions_string)
}
