#' Compile MCMC sampler using STAN and create analysis object
#'
#' @param data_matrix matrix. The data matrix, including all covariates to be
#' adjusted for, all relevant outcome variables, and treatment arm and external
#' control arm flags.
#' @param covariates `Covariate`. Object of class `Covariate` as output by
#' the function `covariate_details()`.
#' @param outcome `Outcome`. Object of class `Outcome` as output by
#' `exp_surv_dist()`, `weib_ph_surv_dist()`, or `logistic_bin_outcome()`.
#' @param borrowing `Borrowing`. Object of class `Borrowing` as output by
#' `borrowing_details()`.
#' @param treatment `Treatment`. Object of class `Treatment` as output by
#' `treatment_details()`.
#'
#' @return Object of class `Analysis`
#' @export
#'
#' @include analysis_class.R
#' @importFrom stats complete.cases
#'
#' @examples
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
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
#'   )
#' )
#'
create_analysis_obj <- function(data_matrix,
                                covariates = NULL,
                                outcome,
                                borrowing,
                                treatment) {
  assert_matrix(data_matrix, mode = "numeric")
  assert_multi_class(covariates, c("Covariates", "NULL"))
  assert_class(outcome, "Outcome")
  assert_class(borrowing, "Borrowing")
  assert_class(treatment, "Treatment")

  ## For now, require all fields (even if not used by model) to be non-missing
  if (any(!complete.cases(data_matrix))) {
    stop(
      "Data matrix must not include any missing data. ",
      "Filter to only complete cases or remove irrelevant columns"
    )
  }

  analysis_obj <- .analysis_obj(
    data_matrix = data_matrix,
    covariates = covariates,
    outcome = outcome,
    borrowing = borrowing,
    treatment = treatment
  )

  check_data_matrix_has_columns(analysis_obj)

  return(analysis_obj)
}
