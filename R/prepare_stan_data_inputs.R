#' Prepare Stan data inputs
#'
#' Create a list of data in the correct format for Stan
#'
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class] created by
#' `create_analysis_obj()`.
#'
#' @return Named list of data inputs that correspond to items in the
#' Stan data {} block.
#'
#' @include create_analysis_obj.R
#' @noRd
#'
#' @examples
#' anls_obj <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100)),
#'   borrowing = borrowing_hierarchical_commensurate(
#'     "ext",
#'     prior_exponential(0.001)
#'   ),
#'   treatment = treatment_details("trt", prior_normal(0, 100))
#' )
#'
#' data_in <- psborrow2:::prepare_stan_data_inputs(anls_obj)
#'
prepare_stan_data_inputs <- function(analysis_obj) {
  ## Trimmed data matrix
  trimmed_data_matrix <- trim_data_matrix(analysis_obj)

  ## Common inputs
  data_in <- list(
    N = NROW(trimmed_data_matrix),
    trt = trimmed_data_matrix[, analysis_obj@treatment@trt_flag_col]
  )

  ## Outcome-specific additions
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    data_in[["time"]] <- trimmed_data_matrix[, analysis_obj@outcome@time_var]
    data_in[["cens"]] <- trimmed_data_matrix[, analysis_obj@outcome@cens_var]
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    data_in[["y"]] <- trimmed_data_matrix[, analysis_obj@outcome@binary_var]
  } else if (is(analysis_obj@outcome, "ContinuousOutcome")) {
    data_in[["y"]] <- trimmed_data_matrix[, analysis_obj@outcome@continuous_var]
  }

  ## BDB additions
  if (is(analysis_obj@borrowing, "BorrowingHierarchicalCommensurate")) {
    data_in[["Z"]] <- cbind(
      1 - trimmed_data_matrix[, analysis_obj@borrowing@ext_flag_col],
      trimmed_data_matrix[, analysis_obj@borrowing@ext_flag_col]
    ) # Column 1 is the indicator for internal. Column 2 is the external indicator.
  }

  ## Covariate additions
  if (!is.null(analysis_obj@covariates)) {
    data_in[["K"]] <- NROW(analysis_obj@covariates@covariates)
    data_in[["X"]] <- trimmed_data_matrix[, analysis_obj@covariates@covariates, drop = FALSE]
    beta_constraints <- get_covariate_constraints(analysis_obj@covariates)
    data_in[["L_beta"]] <- beta_constraints[, "lower"]
    data_in[["U_beta"]] <- beta_constraints[, "upper"]
  }

  ## Weights
  if (analysis_obj@outcome@weight_var != "") {
    data_in[["weight"]] <- trimmed_data_matrix[, analysis_obj@outcome@weight_var]
  }

  return(data_in)
}
