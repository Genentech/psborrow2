#' Create Analysis Object
#'
#' Specify model details to create and compile an MCMC sampler in STAN
#'
#' @param model_matrix model matrix (e.g., as output by `model.matrix()`)
#' @param covariates object of class `Covariate` output by `add_covariates()`
#' @param outcome object of class `Outcomes` output by `exp_surv_dist()`,
#' `weib_ph_surv_dist()`, or `logistic_bin_outcome()`
#' @param borrowing object of class `Borrowing` output by borrowing_details()
#' @param treatment_arms object of class `Treatment` output by
#' `treatment_details()`
#'
#' @return Object of class `Analysis`
#' @export
#'
#' @include analysis_class.R
#'
#' @examples
#' model_matrix <- structure(c(
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
#'   model_matrix = model_matrix,
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
#'     baseline_prior = normal_prior(0, 1000),
#'     "ext",
#'     exponential_prior(.001)
#'   ),
#'   treatment_arms = treatment_details("trt", normal_prior(0, 1000))
#' )
#'
create_analysis_obj <- function(model_matrix,
                                covariates = NULL,
                                outcome,
                                borrowing,
                                treatment_arms) {
  # Check columns exist----
  ## Covariates----
  if (!is.null(covariates)) {
    if (!all(covariates@covariates %in% colnames(model_matrix))) {
      stop(paste0(
        "Covariate columns `",
        paste0(covariates@covariates[
          !covariates@covariates %in% colnames(model_matrix)
        ], collapse = "`, `"),
        "` are not in the model matrix"
      ))
    }
  }

  ## Outcomes----
  if (is(outcome, "TimeToEvent") && !outcome@time_var %in% colnames(model_matrix)) {
    stop(paste0(
      "Time variable `",
      outcome@time_var,
      "` is not a column in the model matrix"
    ))
  }
  if (is(outcome, "TimeToEvent") && !outcome@cens_var %in% colnames(model_matrix)) {
    stop(paste0(
      "Censor variable `",
      outcome@cens_var,
      "` is not a column in the model matrix"
    ))
  }
  if (is(outcome, "BinaryEndpoint") && !outcome@endpoint_var %in% colnames(model_matrix)) {
    stop(paste0(
      "Endpoint variable `",
      outcome@endpoint_var,
      "` is not a column in the model matrix"
    ))
  }

  ## Treatment and external control arms----
  if (borrowing@method == "BDB" && !borrowing@ext_flag_col %in% colnames(model_matrix)) {
    stop(paste0(
      "External flag variable `",
      borrowing@ext_flag_col,
      "` is not a column in the model matrix"
    ))
  }

  if (!treatment_arms@trt_flag_col %in% colnames(model_matrix)) {
    stop(paste0(
      "Treatment flag variable `",
      treatment_arms@trt_flag_col,
      "` is not a column in the model matrix"
    ))
  }

  ## Select only relevant columns in model matrix----
  if (is(outcome, "TimeToEvent")) {
    cols_of_interest <- c(
      treatment_arms@trt_flag_col,
      borrowing@ext_flag_col,
      outcome@time_var,
      outcome@cens_var
    )
  } else if (is(outcome, "BinaryEndpoint")) {
    cols_of_interest <- c(
      treatment_arms@trt_flag_col,
      borrowing@ext_flag_col,
      outcome@endpoint_var
    )
  }

  if (!is.null(covariates)) {
    cols_of_interest <- c(cols_of_interest, covariates@covariates)
  }

  mm <- model_matrix[, cols_of_interest]

  if (borrowing@method == "No borrowing") {
    mm <- mm[mm[, borrowing@ext_flag_col] == 0, ]
    if (sum(mm[, borrowing@ext_flag_col] == 1) > 0) {
      warning("\r", paste0(
        "Removing ",
        sum(mm[, borrowing@ext_flag_col] == 1),
        " patients who were from the",
        " external control cohort because borrowing",
        " type is 'No borrowing'"
      ))
    }
  }

  # Check for correct data types and no missing values----
  if (!is.numeric(mm)) {
    stop("Model matrix must be numeric")
  }

  if (!all(stats::complete.cases(mm))) {
    stop("There are missing values in the model matrix! Right now,
           no methods for missing data are supported")
  }

  # Make analysis object ----
  analysis_obj <- .analysis_obj(
    model_matrix = mm,
    covariates = covariates,
    outcome = outcome,
    borrowing = borrowing,
    treatment_arms = treatment_arms
  )

  message("\r", "Inputs look good", appendLF = FALSE)

  # Write the STAN script----
  message("\r", "Writing STAN program", appendLF = FALSE)
  message("\r", "", appendLF = FALSE)

  ## Function string ----
  function_str <- glue::glue("
      functions {
         {{outcome@function_stan_code}}
      }", .open = "{{", .close = "}}")

  ## Data string ----
  data_str <- glue::glue("data {", .open = "{{", .close = "}}")

  ### Data shared by all TTE
  if (is(outcome, "TimeToEvent")) {
    data_str <- glue::glue("
         {{data_str}}
            int<lower=0> N;
            vector[N] time;
            vector[N] cens;
            vector[N] trt;
         ", .open = "{{", .close = "}}")
  }

  ### Data shared by all binary endpoint
  if (is(outcome, "BinaryEndpoint")) {
    data_str <- glue::glue("
         {{data_str}}
            int<lower=0> N;
            array[N] int y;
            vector[N] trt;
         ", .open = "{{", .close = "}}")
  }

  ### Add external control flag for BDB
  if (borrowing@method == "BDB") {
    data_str <- glue::glue("
                             {{data_str}}
                              matrix[N,2] Z;
                             ",
      .open = "{{",
      .close = "}}"
    )
  }

  ### Add covariates for when these are specified
  if (!is.null(covariates)) {
    data_str <- glue::glue("
            {{data_str}}
            int<lower=0> K;
            matrix[N, K] X;
         ", .open = "{{", .close = "}}")
  }

  ### Close brackets
  data_str <- glue::glue("{{data_str}} }", .open = "{{", .close = "}}")

  ## Parameter string ----
  ### Set parameters shared by all methods
  param_str <- glue::glue("parameters {
                            real beta_trt;
                           ", .open = "{{", .close = "}}")

  ### Set tau and alpha[2] for BDB
  if (borrowing@method == "BDB") {
    param_str <- glue::glue("{{param_str}}
                              real<lower=0> tau;
                              vector[2] alpha;",
      .open = "{{",
      .close = "}}"
    )
  }

  ### Set alpha for non-BDB
  if (borrowing@method != "BDB") {
    param_str <- glue::glue("{{param_str}}
                              real alpha;",
      .open = "{{",
      .close = "}}"
    )
  }

  ### Add outcome specific parameters
  if (NROW(outcome@param_priors) > 0) {
    for (i in seq_len(NROW(outcome@param_priors))) {
      param_str <- glue::glue("{{param_str}}
                                 real {{names(outcome@param_priors[i])}} ;",
        .open = "{{",
        .close = "}}"
      )
    }
  }

  ### Add in vector of coefficients if covariates are provided
  if (!is.null(covariates)) {
    param_str <- glue::glue("{{param_str}}
                               vector[K] beta ;",
      .open = "{{",
      .close = "}}"
    )
  }

  ### Close brackets
  param_str <- glue::glue("{{param_str}} }", .open = "{{", .close = "}}")

  ## Transformed parameters ----
  if (is(outcome, "TimeToEvent")) {
    transf_param_str <- glue::glue("
         transformed parameters {
            real HR_trt = exp(beta_trt);
         }", .open = "{{", .close = "}}")
  } else if (is(outcome, "BinaryEndpoint")) {
    transf_param_str <- glue::glue("
         transformed parameters {
            real OR_trt = exp(beta_trt);
         }", .open = "{{", .close = "}}")
  }

  ## Model string ----
  ### Set values shared by all
  object <- treatment_arms@trt_prior
  beta_trt_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")
  model_str <- glue::glue("model {
                           vector[N] lp;
                           vector[N] elp;
                           beta_trt ~ {{beta_trt_prior}} ;
                           ", .open = "{{", .close = "}}")

  ### Specify different combinations
  if (!is.null(covariates) && borrowing@method == "BDB") {
    model_str <- glue::glue("{{model_str}}
                              lp = X * beta + Z * alpha + trt * beta_trt;
                              elp = exp(lp) ;
                              ",
      .open = "{{",
      .close = "}}"
    )
  } else if (is.null(covariates) && borrowing@method == "BDB") {
    model_str <- glue::glue("{{model_str}}
                              lp = Z * alpha + trt * beta_trt;
                              elp = exp(lp) ;",
      .open = "{{",
      .close = "}}"
    )
  } else if (!is.null(covariates) && borrowing@method != "BDB") {
    model_str <- glue::glue("{{model_str}}
                              lp = alpha + X * beta + trt * beta_trt ;
                              elp = exp(lp) ;",
      .open = "{{",
      .close = "}}"
    )
  } else if (is.null(covariates) && borrowing@method != "BDB") {
    model_str <- glue::glue("{{model_str}}
                              lp = alpha + trt * beta_trt ;
                              elp = exp(lp); ",
      .open = "{{",
      .close = "}}"
    )
  }

  ### Add priors for relevant parameters
  if (NROW(outcome@param_priors) > 0) {
    for (i in seq_len(NROW(outcome@param_priors))) {
      name <- names(outcome@param_priors)[i]
      object <- outcome@param_priors[[name]]
      value <- glue::glue(object@stan_code, .open = "{{", .close = "}}")
      prior_str <- glue::glue("
            {{name}} ~ {{value}} ;", .open = "{{", .close = "}}")
      model_str <- glue::glue("
                                      {{model_str}}
                                      {{prior_str}}
                                      ", .open = "{{", .close = "}}")
    }
  }

  ### Add in tau and alphas if method = BDB
  if (borrowing@method == "BDB") {
    object <- borrowing@tau_prior
    tau_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")

    object <- borrowing@baseline_prior
    alpha_2_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")

    model_str <- glue::glue("
                           {{model_str}}
                           tau ~ {{tau_prior}} ;
                           real sigma;
                           sigma = 1 / tau;
                           alpha[2] ~ {{alpha_2_prior}} ;
                           alpha[1] ~ normal(alpha[2], sqrt(sigma)) ;
                           ", .open = "{{", .close = "}}")
  }

  ### Add in alphas if method is not BDB
  if (borrowing@method != "BDB") {
    object <- borrowing@baseline_prior
    alpha_prior <- glue::glue(object@stan_code, .open = "{{", .close = "}}")

    model_str <- glue::glue("
                           {{model_str}}
                           alpha ~ {{alpha_prior}} ;
                           ", .open = "{{", .close = "}}")
  }

  ### Add in likelihood function
  model_str <- glue::glue("
                           {{model_str}}
                           {{outcome@likelihood_stan_code}}",
    .open = "{{",
    .close = "}}"
  )

  ### Close brackets
  model_str <- glue::glue("{{model_str}} }", .open = "{{", .close = "}}")

  ## Combine model components ----
  model <- glue::glue("
                       {{function_str}}

                       {{data_str}}

                       {{param_str}}

                       {{transf_param_str}}

                       {{model_str}}

                       ", .open = "{{", .close = "}}")

  # Write STAN model and compile ----
  stan_file <- cmdstanr::write_stan_file(model)
  analysis_obj@model_and_data <- list(stan_model = cmdstanr::cmdstan_model(stan_file))
  message("\r", "STAN program compiled successfully", appendLF = FALSE)
  analysis_obj@model_string <- model

  # Prepare data ----
  ## Common inputs
  data_in <- list(
    N = NROW(mm),
    trt = mm[, treatment_arms@trt_flag_col]
  )

  ## TTE additions
  if (is(outcome, "TimeToEvent")) {
    data_in[["time"]] <- mm[, outcome@time_var]
    data_in[["cens"]] <- mm[, outcome@cens_var]
  } else if (is(outcome, "BinaryEndpoint")) {
    data_in[["y"]] <- mm[, outcome@endpoint_var]
  }

  ## BDB additions
  if (borrowing@method == "BDB") {
    data_in[["Z"]] <- matrix(
      c(
        1 - mm[, borrowing@ext_flag_col], # First col is ext = 0
        mm[, borrowing@ext_flag_col]
      ), # Second col is ext = 1
      ncol = 2,
      byrow = FALSE
    )
  }

  ## Covariate additions
  if (!is.null(covariates)) {
    data_in[["K"]] <- NROW(covariates@covariates)
    data_in[["X"]] <- mm[, covariates@covariates]
  }

  # Load data in cmdstanr model
  analysis_obj@model_and_data[["data_in"]] <- data_in
  analysis_obj@ready_to_sample <- TRUE
  message("\r", "Ready to go! Now call mcmc_sample()", appendLF = FALSE)

  # Return
  return(analysis_obj)
}
