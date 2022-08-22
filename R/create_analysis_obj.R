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
#' @param quiet logical. Whether to suppress message (`TRUE`) or not (`FALSE`,
#' the default)
#'
#' @return Object of class `Analysis`
#' @export
#'
#' @include analysis_class.R
#' @importFrom stats complete.cases
#'
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
#' anls <- create_analysis_obj(
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
#'   )
#' )
#'
create_analysis_obj <- function(data_matrix,
                                covariates = NULL,
                                outcome,
                                borrowing,
                                treatment,
                                quiet = FALSE) {
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

  # check data matrix has columns
  psborrow2:::check_data_matrix_has_columns(analysis_obj)

  if (!quiet) {
    message("\r", "Inputs look good", appendLF = FALSE)
  }

  # Trim model matrix
  analysis_obj <- psborrow2:::trim_data_matrix(analysis_obj)

  # Model string components
  functions_str <- psborrow2:::make_model_string_functions(analysis_obj)
  data_str <- psborrow2:::make_model_string_data(analysis_obj)


  # Model string
  model_string <- psborrow2:::h_glue("
    {{functions_str}}
    {{data_str}}

    ")

  analysis_obj@model_string <- model_string

  return(analysis_obj)
}
