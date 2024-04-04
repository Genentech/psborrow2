#' Check Data Matrix for Required Columns
#'
#' Check that an `Analysis` object's `data_matrix` has all the required variables.
#'
#' @param object `Analysis`. Object to check.
#'
#' @return `stop()` if some columns are missing.
#' @export
#' @examples
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
#' check_data_matrix_has_columns(anls)
#'
check_data_matrix_has_columns <- function(object) {
  assert_class(object, "Analysis")
  data_cols <- colnames(object@data_matrix)
  error_col <- c()

  if (!get_vars(object@treatment) %in% data_cols) {
    error_col <- c(error_col, get_vars(object@treatment))
  }

  if (!get_vars(object@borrowing) %in% data_cols) {
    error_col <- c(error_col, get_vars(object@borrowing))
  }

  missing_covs <- setdiff(get_vars(object@covariates), data_cols)
  if (length(missing_covs)) error_col <- c(error_col, covariates = toString(missing_covs))

  if (any(missing_outcomes <- !get_vars(object@outcome) %in% data_cols)) {
    error_col <- c(error_col, get_vars(object@outcome)[missing_outcomes])
  }

  if (length(error_col)) {
    stop(
      "The following specified variables were not found in `data_matrix`:\n",
      paste0("  ", names(error_col), ": ", error_col, "\n")
    )
  }
}
