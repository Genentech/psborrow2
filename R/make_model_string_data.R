#' Make model string of Stan's data block
#'
#' Create the Stan string encompassed by data `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return `glue` `character` containing the text for the data block.
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
#' psborrow2:::make_model_string_data(anls_obj)
#'
make_model_string_data <- function(analysis_obj) {
  ## Data string
  data_string <- h_glue("data {")

  ## Data shared by all TTE
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    data_string <- h_glue("
      {{data_string}}
      int<lower=0> N;
      vector[N] time;
      vector[N] cens;
      vector[N] trt;")
  }

  ### Data shared by all binary endpoints
  if (is(analysis_obj@outcome, "BinaryOutcome")) {
    data_string <- h_glue("
      {{data_string}}
      int<lower=0> N;
      array[N] int y;
      vector[N] trt;")
  }

  ### Add external control flag for BDB
  if (analysis_obj@borrowing@method == "BDB") {
    data_string <- h_glue("
      {{data_string}}
      matrix[N,2] Z;")
  }

  ### Add covariates for when these are specified
  if (!is.null(analysis_obj@covariates)) {
    data_string <- h_glue("
      {{data_string}}
      int<lower=0> K;
      matrix[N, K] X;")
  }

  ### Close block
  data_string <- h_glue("{{data_string}} }")

  ## Return
  return(data_string)
}
