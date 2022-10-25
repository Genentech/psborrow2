#' Create Data Matrix
#'
#' Creates a matrix suitable for [create_analysis_obj()]. Creates dummy variables for factors and
#' allows transformations of covariates specified with a formula.
#'
#' @param data data.frame. Data containing all variables
#' @param outcome character. The outcome variable for binary outcomes or the time and censoring variables.
#' @param trt_flag_col character. The treatment indicator variable.
#' @param ext_flag_col character. The external cohort indicator.
#' @param covariates character or formula. The covariates for model adjustment.
#' @param weight_var character. An optional weight variable.
#'
#' @return  Invisibly returns a `matrix` containing all variables to pass to [create_analysis_obj()].
#'  Prints names of covariates columns to use with [add_covariates()].
#' @export
#'
#' @examples
#' dat <- survival::diabetic
#' dat$ext <- dat$trt == 0 & dat$id > 1000
#' data_mat <- create_data_matrix(
#'   dat,
#'   outcome = c("time", "status"),
#'   trt_flag_col = "trt",
#'   ext_flag_col = "ext",
#'   covariates = ~ age + laser + log(risk)
#' )
#' data_mat
create_data_matrix <- function(data, outcome, trt_flag_col, ext_flag_col, covariates = NULL, weight_var = NULL) {
  assert_data_frame(data)
  data_cols <- colnames(data)
  assert_character(outcome, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_subset(outcome, data_cols)

  assert_character(trt_flag_col, len = 1, any.missing = FALSE)
  assert_subset(trt_flag_col, data_cols)

  assert_character(ext_flag_col, len = 1, any.missing = FALSE)
  assert_subset(ext_flag_col, data_cols)

  assert_character(weight_var, len = 1, any.missing = FALSE, null.ok = TRUE)
  assert_subset(weight_var, data_cols)

  flag_outcome_formula <- formula(paste("~", paste(c(outcome, trt_flag_col, ext_flag_col, weight_var), collapse = "+")))
  output_matrix <- model.matrix(flag_outcome_formula, data)[, -1]

  if (!is.null(covariates)) {
    assert_multi_class(covariates, c("character", "formula"))
    if (test_formula(covariates)) {
      assert_subset(all.vars(covariates), data_cols)
    } else if (is.character(covariates)) {
      assert_subset(covariates, data_cols)
      covariates <- formula(paste("~", paste(covariates, collapse = "+")))
    }
    covariates_matrix <- model.matrix(covariates, data)[, -1, drop = FALSE]
    matrix_cov_cols <- deparse(colnames(covariates_matrix))
    cat("Call `add_covariates()` with `covariates = ", matrix_cov_cols, "`\n")
    output_matrix <- cbind(output_matrix, covariates_matrix)
  }
  invisible(output_matrix)
}
