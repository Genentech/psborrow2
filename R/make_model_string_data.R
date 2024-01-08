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
#'   outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
#'   borrowing = borrowing_full("ext"),
#'   treatment = treatment_details("trt", prior_normal(0, 100))
#' )
#'
#' psborrow2:::make_model_string_data(anls_obj)
#'
make_model_string_data <- function(analysis_obj) {
  outcome_string <- analysis_obj@outcome@data_stan_code

  borrowing_string <- analysis_obj@borrowing@data_stan_code

  covariate_string <- ifelse(
    analysis_obj@covariates@n_covs > 0,
    h_glue("int<lower=0> K;
     matrix[N, K] X;
     vector[K] L_beta;
     vector[K] U_beta;
     "),
    ""
  )

  data_string <- h_glue("data {
    int<lower=0> N;
    vector[N] trt;
    {{outcome_string}}
    {{borrowing_string}}
    {{covariate_string}}
  }")

  data_string
}
