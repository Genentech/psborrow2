
#' Check Data Matrix for Required Columns
#'
#' Check that an `Analysis` object's `data_matrix` has all the required variables.
#'
#' @param object `Analysis`. Object to check.
#'
#' @return `stop()` if some columns are missing.
#' @export
#' @examples
#' data_matrix <- structure(c(
#'   1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
#'   1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
#'   0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1,
#'   0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1,
#'   1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1,
#'   1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0,
#'   1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 5.32295081977934,
#'   6.96715560527452, 1.17501259866481, 9.45936763681621, 5.75572041253912,
#'   12.1139661284359, 2.64741266341488, 4.99828513121648, 5.38734198746281,
#'   4.74770899862051, 0.0803900761309156, 13.7720370325053, 3.03310634382069,
#'   10.1695853577489, 0.0720591936260462, 10.1367262049345, 2.9709762107209,
#'   0.659847613424063, 3.88436722227683, 3.2750634373027, 1.90838416890977,
#'   5.79706331825161, 4.28611800974856, 0.702194716266679, 4.74582234003252,
#'   6.92417557015123, 6.53942201171797, 5.88460493011677, 1.84311583921956,
#'   5.28505285794622, 4.34498298102206, 3.17685930818209, 11.0179639531233,
#'   2.14560192144267, 4.40741405311895, 10.9576044368026, 3.55944875309522,
#'   9.07620135719862, 1.29542022943497, 3.35630633204141, 14.1141011930051,
#'   14.3560852138326, 6.76962562138734, 6.60672739803918, 0.727092696356863,
#'   3.06457582335024, 2.27240795704226, 6.12868075434827, 7.45796004200603,
#'   9.23882804838511, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
#'   0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1,
#'   1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0
#' ), dim = c(50L, 6L), dimnames = list(
#'   NULL, c("ext", "trt", "cov1", "cov2", "time", "cnsr")
#' ))
#'
#' anls <- psborrow2:::.analysis_obj(
#'   data_matrix = data_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = normal_prior(0, 1000)
#'   ),
#'   outcome = exp_surv_dist(
#'     "time",
#'     "cnsr"
#'   ),
#'   borrowing = borrowing_details(
#'     "BDB",
#'     "ext",
#'     exponential_prior(.001),
#'     baseline_prior = normal_prior(0, 1000)
#'   ),
#'   treatment = treatment_details(
#'     "trt",
#'     normal_prior(0, 1000)
#'   ),
#'   ready_to_sample = FALSE
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

  missing_outcomes <- setdiff(get_vars(object@outcome), data_cols)
  if (length(missing_outcomes)) error_col <- c(error_col, outcome = toString(missing_outcomes))

  if (length(error_col)) {
    stop(
      "The following specified variables were not found in `data_matrix`:\n",
      paste0("  ", names(error_col), ": ", error_col, "\n")
    )
  }
}
