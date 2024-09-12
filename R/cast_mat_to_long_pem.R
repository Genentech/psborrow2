#' Cast data to "long" format for PEM
#'
#' Take an `Analysis` object's `data_matrix` and convert it to "long" format, one
#' row per patient per period
#'
#' @param analysis_obj `Analysis`. An object of class `Analysis`
#' as created through `.create_analysis_obj()`.
#'
#' @return Long data matrix
#' @noRd
#' @examples
#'
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = prior_normal(0, 1000)
#'   ),
#'   outcome = outcome_surv_pem(
#'     "time",
#'     "cnsr",
#'     baseline_prior = prior_normal(0, 1000),
#'     cut_points = c(1,2,3)
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
#' trimmed_mat <- psborrow2:::cast_mat_to_long_pem(anls)
#'
cast_mat_to_long_pem <- function(analysis_obj) {

  ## Start with data.frame
  df <- as.data.frame(analysis_obj@data_matrix)

  ## Check cut points
  cut_points <- analysis_obj@outcome@cut_points
  max_fup <- max(df[,analysis_obj@outcome@time_var])
  cut_points_keep <- cut_points[cut_points <= max_fup]
  if (length(cut_points_keep) < length(cut_points)) {
    warning(paste0("Some cut points are greater than the maximum follow-up time of ", max_fup, ". These will be ignored."))
  }

  ## Create long data

}
