#' Prepare Stan data inputs
#'
#' Create a list of data in the correct format for Stan
#'
#' @param borrowing_obj `Borrowing`. Object of class [`Borrowing`](Borrowing-class).
#' @param outcome_obj `Outcome`. Object of class [`Outcome`](Outcome-class).
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class].
#'
#' @return Named list of data inputs that correspond to items in the
#' Stan data {} block.
#'
#' @include create_analysis_obj.R load_and_interpolate_stan_model.R
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
#' data_in <- psborrow2:::prepare_stan_data_inputs(anls_obj@outcome, anls_obj@borrowing, anls_obj)
#'
setGeneric(
  "prepare_stan_data_inputs",
  function(outcome, borrowing, analysis_obj) standardGeneric("prepare_stan_data_inputs")
)

# Helper function to add covariates and weights to prepare stan data inputs
#'
#' Add covariate and weight data to the Stan data inputs
#'
#' @param data_in Named list of data inputs for Stan.
#' @param analysis_obj `Analysis`. Object of class [`Analysis`][Analysis-class].
#' @param data_matrix Data matrix from the analysis object.
#'
#' @return Named list of data inputs with covariates and weights added.
#' @noRd
add_covariates_and_weights <- function(data_in, analysis_obj, data_matrix) {

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

# class union ----
setClassUnion("OutcomeSurvExponentialWeibull", c("OutcomeSurvExponential", "OutcomeSurvWeibullPH"))

# Survival ----
## Exponential / Weibull ----
### No/full borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeSurvExponentialWeibull", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- trim_data_matrix(analysis_obj)
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      time = data_matrix[, outcome@time_var],
      cens = data_matrix[, outcome@cens_var]
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeSurvExponentialWeibull", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- analysis_obj@data_matrix
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      time = data_matrix[, outcome@time_var],
      cens = data_matrix[, outcome@cens_var],
      Z = cbind(
        1 - data_matrix[, borrowing@ext_flag_col],
        data_matrix[, borrowing@ext_flag_col]
      )
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

# Survival ----
## PEM ----
### No/full borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeSurvPEM", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    analysis_obj@data_matrix <- trim_data_matrix(analysis_obj)
    analysis_obj <- cast_mat_to_long_pem(analysis_obj)
    data_matrix <- analysis_obj@data_matrix

    n_periods <- analysis_obj@outcome@n_periods
    Z <- matrix(0, nrow = nrow(data_matrix), ncol = n_periods)
    for (i in seq_len(nrow(data_matrix))) {
      period <- data_matrix[i, "__period__"]
      Z[i, period] <- 1
    }
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      time = data_matrix[, outcome@time_var],
      cens = data_matrix[, outcome@cens_var],
      N_periods = max(data_matrix[, "__period__"]),
      Z = Z
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

### Hierarchical Commensurate Borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeSurvPEM", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    analysis_obj <- cast_mat_to_long_pem(analysis_obj)
    data_matrix <- analysis_obj@data_matrix

    n_periods <- analysis_obj@outcome@n_periods
    Z0 <- Z1 <- matrix(0, nrow = nrow(data_matrix), ncol = n_periods)
    for (i in seq_len(nrow(data_matrix))) {
      period <- data_matrix[i, "__period__"]
      Z0[i, period] <- data_matrix[i, "ext"]
      Z1[i, period] <- 1 - data_matrix[i, "ext"]
    }

    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      time = data_matrix[, outcome@time_var],
      cens = data_matrix[, outcome@cens_var],
      N_periods = max(data_matrix[, "__period__"]),
      Z0 = Z0,
      Z1 = Z1
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

# Binary ----
## Logistic ----
### No/full borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeBinaryLogistic", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- trim_data_matrix(analysis_obj)
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      y = data_matrix[, outcome@binary_var]
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeBinaryLogistic", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- analysis_obj@data_matrix
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      y = data_matrix[, outcome@binary_var],
      Z = cbind(
        1 - data_matrix[, borrowing@ext_flag_col],
        data_matrix[, borrowing@ext_flag_col]
      )
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

# Continuous ----
## Normal ----
### No/full borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeContinuousNormal", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- trim_data_matrix(analysis_obj)
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      y = data_matrix[, outcome@continuous_var]
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "prepare_stan_data_inputs",
  signature = c("OutcomeContinuousNormal", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    data_matrix <- analysis_obj@data_matrix
    data_in <- list(
      N = nrow(data_matrix),
      trt = data_matrix[, analysis_obj@treatment@trt_flag_col],
      y = data_matrix[, outcome@continuous_var],
      Z = cbind(
        1 - data_matrix[, borrowing@ext_flag_col],
        data_matrix[, borrowing@ext_flag_col]
      )
    )

    # Add covariates and weights
    data_in <- add_covariates_and_weights(data_in, analysis_obj, data_matrix)

    return(data_in)
  }
)
