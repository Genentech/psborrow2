#' Build the model string by interpolating the Stan template
#' @param template_domain Character string specifying the domain of the template (e.g., "surv", "bin", "cont")
#' @param template_filename Character string specifying the filename of the Stan template
#' @param outcome `Outcome` object
#' @param borrowing `Borrowing` object
#' @param analysis_obj `Analysis` object
#' @param ... Additional named arguments to be passed for interpolation
#' @return String containing the interpolated Stan model
#' @include outcome_surv_pem.R
build_model_string <- function(template_domain, template_filename, outcome, borrowing, analysis_obj, ...) {
  
  # Load the Stan template
  template <- load_stan_file(template_domain, template_filename)

  # Common interpolations
  model_string <- h_glue(
    template,
    weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
    cov.data = if (!is.null(analysis_obj@covariates)) {
      h_glue("
        int<lower=0> K;
        matrix[N, K] X;
        vector[K] L_beta;
        vector[K] U_beta;
      ")
    } else {
      ""
    },
    cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
    trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
    cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
    cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
    weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
    baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
    ... # additional interpolation variables
  )

  return(model_string)
}

# class union ----
setClassUnion("BorrowingNoneFull", c("BorrowingFull", "BorrowingNone"))

#' Load and interpolate Stan model
#' @param outcome `Outcome` object
#' @param borrowing `Borrowing` object
#' @param analysis_obj `Analysis` object
#' @include analysis_class.R outcome_bin_logistic.R outcome_cont_normal.R outcome_surv_exponential.R
#'   outcome_surv_weibull_ph.R
#' @return String containing the interpolated Stan model
setGeneric(
  "load_and_interpolate_stan_model",
  function(outcome, borrowing, analysis_obj) standardGeneric("load_and_interpolate_stan_model")
)

# Survival ----
## Exponential ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvExponential", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "exp_nb.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj
    )

    return(model_string)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvExponential", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "exp_hcp.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)
  }
)

## Weibull Proportional Hazards ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvWeibullPH", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "weib_ph_nb.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      shape.prior = h_glue("shape_weibull ~ {{get_prior_string(outcome@param_priors$shape_weibull)}} ;")
    )

    return(model_string)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvWeibullPH", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "weib_ph_hcp.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      shape.prior = h_glue("shape_weibull ~ {{get_prior_string(outcome@param_priors$shape_weibull)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)
  }
)

## Piecewise exponential ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvPEM", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "pem_nb.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj
    )

    return(model_string)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvPEM", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "surv",
      template_filename = "pem_hcp.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)
  }
)


# Binary ----
## Logistic ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeBinaryLogistic", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "bin",
      template_filename = "logit_nb.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj
    )

    return(model_string)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeBinaryLogistic", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "bin",
      template_filename = "logit_hcp.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)
  }
)


# Continuous ----
## Normal ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeContinuousNormal", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "cont",
      template_filename = "gauss_nb.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      stdev.prior = h_glue("std_dev_outcome ~ {{get_prior_string(outcome@param_priors$std_dev_outcome)}} ;")
    )

    return(model_string)
  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeContinuousNormal", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {
    model_string <- build_model_string(
      template_domain = "cont",
      template_filename = "gauss_hcp.stan",
      outcome = outcome,
      borrowing = borrowing,
      analysis_obj = analysis_obj,
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      stdev.prior = h_glue("std_dev_outcome ~ {{get_prior_string(outcome@param_priors$std_dev_outcome)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)
  }
)
