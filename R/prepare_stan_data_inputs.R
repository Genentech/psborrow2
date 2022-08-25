#' Prepare Stan data inputs
#'
#' Create a list of data in the correct format for Stan
#'
#' @param analysis_obj `Analysis`. Object of class `Analysis` created by
#' `psborrow2:::.analysis_obj()`.
#'
#' @return named list of data inputs that correspond to items in the
#' Stan data {} block
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
#'     "BDB",
#'     normal_prior(0, 100),
#'     "extTRUE",
#'     exponential_prior(0.001)
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 100))
#' )
#'
#' anls_obj@model_and_data <- list(
#'   data_in = psborrow2:::prepare_stan_data_inputs(anls_obj)
#' )
#'
prepare_stan_data_inputs <- function(analysis_obj) {
  ## Common inputs
  data_in <- list(
    N = NROW(analysis_obj@data_matrix),
    trt = analysis_obj@data_matrix[, analysis_obj@treatment@trt_flag_col]
  )

  ## Outcome-specific additions
  if (is(analysis_obj@outcome, "TimeToEvent")) {
    data_in[["time"]] <- analysis_obj@data_matrix[, analysis_obj@outcome@time_var]
    data_in[["cens"]] <- analysis_obj@data_matrix[, analysis_obj@outcome@cens_var]
  } else if (is(analysis_obj@outcome, "BinaryOutcome")) {
    data_in[["y"]] <- analysis_obj@data_matrix[, analysis_obj@outcome@binary_var]
  }

  ## BDB additions
  if (analysis_obj@borrowing@method == "BDB") {
    data_in[["Z"]] <- cbind(
      1 - analysis_obj@data_matrix[, analysis_obj@borrowing@ext_flag_col],
      analysis_obj@data_matrix[, analysis_obj@borrowing@ext_flag_col]
    ) # Column 1 is the indicator for internal, column 2 is the external indicator.
  }

  ## Covariate additions
  if (!is.null(analysis_obj@covariates)) {
    data_in[["K"]] <- NROW(analysis_obj@covariates@covariates)
    data_in[["X"]] <- analysis_obj@data_matrix[
      , analysis_obj@covariates@covariates
    ]
  }

  return(data_in)
}
