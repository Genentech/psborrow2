#' Compile MCMC sampler using STAN and create analysis object
#'
#' @param data_matrix matrix. The data matrix, including all covariates to be
#' adjusted for, all relevant outcome variables, and treatment arm and external
#' control arm flags.
#' @param covariates `Covariates`. Object of class [`Covariates`][Covariates-class] as output by
#' the function [`add_covariates()`].
#' @param outcome `Outcome`. Object of class `Outcome`[Outcome-class] as output by
#' [`exp_surv_dist()`], [`weib_ph_surv_dist()`], or [`logistic_bin_outcome()`].
#' @param borrowing `Borrowing`. Object of class [`Borrowing`][Borrowing-class] as output by
#' [`borrowing_details()`].
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
  check_data_matrix_has_columns(analysis_obj)

  if (!quiet) {
    message("\r",
      "Inputs look good.",
      appendLF = TRUE
    )


    if (analysis_obj@borrowing@method == "Full borrowing") {
      message("\r",
        glue::glue("NOTE: dropping column `{analysis_obj@borrowing@ext_flag_col}` for full borrowing."),
        appendLF = TRUE
      )
    }

    if (analysis_obj@borrowing@method == "No borrowing") {
      message("\r",
        glue::glue("NOTE: excluding `{analysis_obj@borrowing@ext_flag_col}` == `1`/`TRUE` for no borrowing."),
        appendLF = TRUE
      )
    }
  }

  # Trim model matrix
  analysis_obj <- trim_data_matrix(analysis_obj)

  # Model string components
  functions_str <- make_model_string_functions(analysis_obj)
  data_str <- make_model_string_data(analysis_obj)
  param_str <- make_model_string_parameters(analysis_obj)
  trans_param_str <- make_model_string_transf_param(analysis_obj)
  model_str <- make_model_string_model(analysis_obj)

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
  stan_file <- write_stan_file(analysis_obj@model_string)
  if (!quiet) {
    analysis_obj@model_and_data <- list(stan_model = cmdstan_model(stan_file))
  } else if (quiet) {
    suppressMessages(
      analysis_obj@model_and_data <- list(stan_model = cmdstan_model(stan_file))
    )
  }

  if (!quiet) {
    message("\r",
      "Stan program compiled successfully!",
      appendLF = TRUE
    )
  }

  # Prepare data inputs
  analysis_obj@model_and_data[["data_in"]] <- prepare_stan_data_inputs(analysis_obj)
  analysis_obj@ready_to_sample <- TRUE
  if (!quiet) {
    message("\r",
      "Ready to go! Now call `mcmc_sample()`.",
      appendLF = TRUE
    )
  }

  return(analysis_obj)
}
