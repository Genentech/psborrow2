#' Create Data Matrix
#'
#' @param data a `data.frame` containing all variables
#' @param outcome (`character`) the outcome variable for binary outcomes or the time and censoring variables
#' @param trt_flag_col (`character`) the treatment indicator variable
#' @param ext_flag_col (`character`) the external cohort indicator
#' @param covariates (`character` or `formula`) the covariates for model adjustment
#'
#' @return  A `matrix` containing all variables to pass to [create_analysis_object()].
#' @export
#'
#' @examples
#' dat <- survival::diabetic
#' dat$ext <- dat$trt == 0 & dat$id > 1000
#' create_data_matrix(
#'   dat,
#'   outcome = c("time", "status"),
#'   trt_flag_col = "trt",
#'   ext_flag_col = "ext",
#'   covariates = ~ age + laser + risk
#' )
create_data_matrix <- function(data, outcome, trt_flag_col, ext_flag_col, covariates = NULL) {
  assert_data_frame(data)
  data_cols <- colnames(data)
  assert_character(outcome, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_subset(outcome, data_cols)

  assert_character(trt_flag_col, len = 1, any.missing = FALSE)
  assert_subset(trt_flag_col, data_cols)

  assert_character(ext_flag_col, len = 1, any.missing = FALSE)
  assert_subset(ext_flag_col, data_cols)

  flag_outcome_formula <- formula(paste("~", paste(c(outcome, trt_flag_col, ext_flag_col), collapse = "+")))
  output_matrix <- model.matrix(flag_outcome_formula, data)[, -1]

  if (!is.null(covariates)) {
    assert_multi_class(covariates, c("character", "formula"))
    if (test_formula(covariates)) {
      assert_subset(all.vars(covariates), data_cols)
    } else if (is.character(covariates)) {
      assert_subset(covariates, data_cols)
      covariates <- formula(paste("~", paste(covariates, collapse = "+")))
    }
    covariates_matrix <- model.matrix(covariates, data)[, -1]
    matrix_cov_cols <- deparse(colnames(covariates_matrix))
    cat("Call `add_covariates()` with `covariates = ", matrix_cov_cols, "`\n")
    output_matrix <- cbind(output_matrix, covariates_matrix)
  }
  output_matrix
}
