#' Compile MCMC sampler using STAN and create analysis object
#'
#' @param data_matrix matrix. The data matrix, including all covariates to be
#' adjusted for, all relevant outcome variables, and treatment arm and external
#' control arm flags.
#' @param covariates `Covariates`. Object of class [`Covariates`][Covariates-class] as output by
#' the function [`add_covariates()`].
#' @param outcome `Outcome`. Object of class [`Outcome`][Outcome-class] as output by
#' [`outcome_surv_exponential()`], [`outcome_surv_weibull_ph()`], or [`outcome_bin_logistic()`].
#' @param borrowing `Borrowing`. Object of class [`Borrowing`][Borrowing-class] as output by
#' [`borrowing_full()`], [`borrowing_none()`], and [`borrowing_hierarchical_commensurate()`].
#' @param treatment `Treatment`. Object of class [`Treatment`][Treatment-class] as output by
#' [`treatment_details()`].
#' @param quiet logical. Whether to suppress messages (`TRUE`) or not (`FALSE`,
#' the default)
#'
#' @return Object of class [`Analysis`][Analysis-class].
#' @export
#'
#' @include analysis_class.R
#' @importFrom stats complete.cases
#'
#' @examples
#' if (check_cmdstan()) {
#'   anls <- create_analysis_obj(
#'     data_matrix = example_matrix,
#'     outcome = outcome_surv_exponential(
#'       "time",
#'       "cnsr",
#'       baseline_prior = prior_normal(0, 1000)
#'     ),
#'     borrowing = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_exponential(.001)
#'     ),
#'     treatment = treatment_details(
#'       "trt",
#'       prior_normal(0, 1000)
#'     ),
#'     covariates = add_covariates(
#'       covariates = c("cov1", "cov2"),
#'       priors = prior_normal(0, 1000)
#'     )
#'   )
#' }
#'
create_analysis_obj <- function(data_matrix,
                                outcome,
                                borrowing,
                                treatment,
                                covariates = NULL,
                                quiet = FALSE) {
  assert_matrix(data_matrix, mode = "numeric")
  assert_multi_class(covariates, c("Covariates", "NULL"))
  assert_class(outcome, "Outcome")
  assert_class(borrowing, "Borrowing")
  assert_class(treatment, "Treatment")
  assert_flag(quiet)

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
  check_data_matrix_has_columns(analysis_obj)

  if (!quiet) {
    message("Inputs look good.")

    if (is(analysis_obj@borrowing, "BorrowingFull")) {
      message(
        h_glue("NOTE: dropping column `{{analysis_obj@borrowing@ext_flag_col}}` for full borrowing.")
      )
    }

    if (is(analysis_obj@borrowing, "BorrowingNone")) {
      message(
        h_glue("NOTE: excluding `{{analysis_obj@borrowing@ext_flag_col}}` == `1`/`TRUE` for no borrowing.")
      )
    }
  }

  # Model string components
  functions_str <- make_model_string_functions(analysis_obj)
  data_str <- make_model_string_data(analysis_obj)
  param_str <- make_model_string_parameters(analysis_obj)
  trans_param_str <- make_model_string_transf_param(analysis_obj)
  model_str <- make_model_string_model(analysis_obj@borrowing, analysis_obj@outcome, analysis_obj)

  # Model string
  stan_model_string <- h_glue("

    {{functions_str}}

    {{data_str}}

    {{param_str}}

    {{trans_param_str}}

    {{model_str}}

  ")

  analysis_obj@model_string <- stan_model_string

  # Compile model
  if (check_cmdstan()) {
    stan_file <- cmdstanr::write_stan_file(analysis_obj@model_string)
    if (!quiet) {
      analysis_obj@model <- cmdstanr::cmdstan_model(stan_file)
    } else if (quiet) {
      suppressMessages(
        analysis_obj@model <- cmdstanr::cmdstan_model(stan_file)
      )
    }

    if (!quiet) {
      message("Stan program compiled successfully!")
    }

    # Prepare data inputs
    analysis_obj@ready_to_sample <- TRUE
    if (!quiet) {
      message("Ready to go! Now call `mcmc_sample()`.")
    }
  } else {
    warning(
      "Stan program could not be compiled as cmdstanr is not available.",
      call. = FALSE
    )
    analysis_obj@ready_to_sample <- FALSE
  }

  return(analysis_obj)
}
