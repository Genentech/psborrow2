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
  outcome_string <- if (is(analysis_obj@outcome, "TimeToEvent")) {
    h_glue("vector[N] time;
     vector[N] cens;")
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    "array[N] int y;"
  }

  borrowing_string <- ifelse(analysis_obj@borrowing@method == "BDB", "matrix[N,2] Z;", "")

  covariate_string <- ifelse(
    !is.null(analysis_obj@covariates),
    h_glue("int<lower=0> K;
     matrix[N, K] X;"),
    ""
  )

  data_string <- h_glue("data {
    int<lower=0> N;
    vector[N] trt;
    {{outcome_string}}
    {{borrowing_string}}
    {{covariate_string}}
  }")

  return(data_string)
}
