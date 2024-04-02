#' Make model string of Stan's parameters model block
#'
#' Create the Stan string encompassed by parameters `{}`
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
#' make_model_string_parameters(anls_obj)
#' @noRd
make_model_string_parameters <- function(analysis_obj) {
  ## Parameters string
  trt_string <- h_glue("real{{eval_constraints(analysis_obj@treatment@trt_prior)}} beta_trt;")

  is_bdb <- isTRUE(is(analysis_obj@borrowing, "BorrowingHierarchicalCommensurate"))
  ### Set tau
  borrowing_string <- if (is_bdb) h_glue("real{{eval_constraints(analysis_obj@borrowing@tau_prior)}} tau;") else ""

  ### Set alpha
  intercept_string <- h_glue(
    "{{type}}{{constraint}}{{n}} alpha;",
    type = if (is_bdb) "vector" else "real",
    constraint = eval_constraints(analysis_obj@outcome@baseline_prior),
    n = if (is_bdb) "[2]" else ""
  )

  ### Add outcome specific parameters
  if (NROW(analysis_obj@outcome@param_priors) > 0) {
    constraints <- lapply(analysis_obj@outcome@param_priors, function(p) eval_constraints(p))
    outcome_string <- h_glue("real{{constraints}} {{names(constraints)}};")
  } else {
    outcome_string <- analysis_obj@outcome@param_stan_code
  }

  ### Add in vector of coefficients if covariates are provided
  covariate_string <- if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else ""

  h_glue("parameters {
  {{trt_string}}
  {{outcome_string}}
  {{borrowing_string}}
  {{intercept_string}}
  {{covariate_string}}
  }")
}
