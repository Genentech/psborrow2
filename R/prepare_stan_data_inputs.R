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
  data_matrix <- trim_data_matrix(analysis_obj)

  ## Cast to long 
  if (is(analysis_obj@outcome, "OutcomeSurvPEM")) {
    data_matrix <- cast_mat_to_long_pem(analysis_obj)
  }

  ## Common inputs
  data_in <- list(
    N = NROW(data_matrix),
    trt = data_matrix[, analysis_obj@treatment@trt_flag_col]
  )

  ## Outcome-specific additions
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    data_in[["time"]] <- data_matrix[, analysis_obj@outcome@time_var]
    data_in[["cens"]] <- data_matrix[, analysis_obj@outcome@cens_var]
    if (is(analysis_obj@outcome, "OutcomeSurvPEM")) {
      data_in[["period"]] <- data_matrix[, "__period__"]
      data_in[["n_periods"]] <-  max(data_matrix[, "__period__"])
    } else {
      data_in[["n_periods"]] <- 1
    }
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    data_in[["y"]] <- data_matrix[, analysis_obj@outcome@binary_var]
  } else if (is(analysis_obj@outcome, "ContinuousOutcome")) {
    data_in[["y"]] <- data_matrix[, analysis_obj@outcome@continuous_var]
  }

  ## BDB additions
  if (is(analysis_obj@borrowing, "BorrowingHierarchicalCommensurate")) {
    data_in[["Z"]] <- cbind(
      1 - data_matrix[, analysis_obj@borrowing@ext_flag_col],
      data_matrix[, analysis_obj@borrowing@ext_flag_col]
    ) # Column 1 is the indicator for internal. Column 2 is the external indicator.
  }

  ## Covariate additions
  if (!is.null(analysis_obj@covariates)) {
    data_in[["K"]] <- NROW(analysis_obj@covariates@covariates)
    data_in[["X"]] <- data_matrix[, analysis_obj@covariates@covariates, drop = FALSE]
    beta_constraints <- get_covariate_constraints(analysis_obj@covariates)
    data_in[["L_beta"]] <- beta_constraints[, "lower"]
    data_in[["U_beta"]] <- beta_constraints[, "upper"]
  }

  ## Weights
  if (analysis_obj@outcome@weight_var != "") {
    data_in[["weight"]] <- data_matrix[, analysis_obj@outcome@weight_var]
  }

  return(data_in)
}
