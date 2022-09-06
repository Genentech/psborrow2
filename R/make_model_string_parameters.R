#' Make model string of Stan's parameters model block
#'
#' Create the Stan string encompassed by parameters `{}`
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return `glue` `character` containing the Stan code for the functions block.
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
#' psborrow2:::make_model_string_parameters(anls_obj)
#'
make_model_string_parameters <- function(analysis_obj) {
  ## Parameters string
  parameters_string <- h_glue(
    "parameters {
    real{{constraint}} beta_trt;",
    constraint = analysis_obj@treatment@trt_prior@constraint
  )

  ### Set tau and alpha[2] for BDB
  if (analysis_obj@borrowing@method == "BDB") {
    parameters_string <- h_glue("
      {{parameters_string}}
      real {{tau_constraint}} tau;
      vector{{alpha_constraint}}[2] alpha;",
      alpha_constraint = analysis_obj@borrowing@baseline_prior@constraint,
      tau_constraint = analysis_obj@borrowing@tau_prior@constraint
    )
  }

  ### Set alpha for non-BDB
  if (analysis_obj@borrowing@method != "BDB") {
    parameters_string <- h_glue("
      {{parameters_string}}
      real{{constraint}} alpha;",
      constraint = analysis_obj@borrowing@baseline_prior@constraint
    )
  }

  ### Add outcome specific parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    for (name in names(analysis_obj@outcome@param_priors)) {
      parameters_string <- h_glue(
        "{{parameters_string}}
        real{{constraint}} {{name}};",
        constraint = analysis_obj@outcome@param_priors[[name]]@constraint
      )
    }
  }

  ### Add in vector of coefficients if covariates are provided
  if (!is.null(analysis_obj@covariates)) {
    parameters_string <- h_glue("
      {{parameters_string}}
      vector<lower=L_beta, upper=U_beta>[K] beta ;")
  }

  ### Close block
  parameters_string <- h_glue("{{parameters_string}} }")

  # Return
  return(parameters_string)
}
