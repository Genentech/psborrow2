#' Trim model matrix to just include relevant columns
#'
#' Take an `Analysis` object's `data_matrix` and remove
#' everything unnecessary in preparation of making input data for Stan
#'
#' @param analysis_obj `Analysis`. An object of class `Analysis`
#' as created through `.create_analysis_obj()`.
#'
#' @return Trimmed data matrix
#' @noRd
#' @examples
#'
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = prior_normal(0, 1000)
#'   ),
#'   outcome = outcome_surv_exponential(
#'     "time",
#'     "cnsr",
#'     baseline_prior = prior_normal(0, 1000)
#'   ),
#'   borrowing = borrowing_hierarchical_commensurate(
#'     "ext",
#'     prior_exponential(.001)
#'   ),
#'   treatment = treatment_details(
#'     "trt",
#'     prior_normal(0, 1000)
#'   )
#' )
#'
#' trimmed_mat <- psborrow2:::trim_data_matrix(anls)
#'
trim_data_matrix <- function(analysis_obj) {
  required_rows <- trim_rows(analysis_obj@borrowing, analysis_obj)
  required_cols <- trim_cols(analysis_obj@borrowing, analysis_obj)
  return(analysis_obj@data_matrix[required_rows, required_cols])
}
