#' Trim model matrix to just include relevant columns
#'
#' Take an `Analysis` object's `data_matrix` and remove
#' everything unnecessary in preparation of making input data for Stan
#'
#' @param analysis_obj `Analysis`. An object of class `Analysis`
#' as created through `.create_analysis_obj()`.
#'
#' @return Trimmed data matrix
#'
#' @examples
#'
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = normal_prior(0, 1000)
#'   ),
#'   outcome = exp_surv_dist(
#'     "time",
#'     "cnsr",
#'     baseline_prior = normal_prior(0, 1000)
#'   ),
#'   borrowing = borrowing_details(
#'     "BDB",
#'     "ext",
#'     exponential_prior(.001)
#'   ),
#'   treatment = treatment_details(
#'     "trt",
#'     normal_prior(0, 1000)
#'   ),
#'   ready_to_sample = FALSE
#' )
#'
#' trimmed_mat <- psborrow2:::trim_data_matrix(anls)
#'
trim_data_matrix <- function(analysis_obj) {
  required_rows <- if (analysis_obj@borrowing@method == "No borrowing") {
    !as.logical(analysis_obj@data_matrix[, get_vars(analysis_obj@borrowing)])
  } else {
    seq_len(NROW(analysis_obj@data_matrix))
  }

  required_cols <- if (analysis_obj@borrowing@method != "BDB") {
    setdiff(get_vars(analysis_obj), get_vars(analysis_obj@borrowing))
  } else {
    get_vars(analysis_obj)
  }

  return(analysis_obj@data_matrix[required_rows, required_cols])
}
