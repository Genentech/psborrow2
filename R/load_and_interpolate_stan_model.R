# class union ----
setClassUnion("BorrowingNoneFull", c("BorrowingFull", "BorrowingNone"))

#' Load and interpolate Stan model
#' @param object `Outcome` object
#' @param borrowing `Borrowing` object
#' @param analysis_obj `Analysis` object
#' @include analysis_class.R outcome_bin_logistic.R outcome_cont_normal.R outcome_surv_exponential.R outcome_surv_weibull_ph.R
#' @return String containing the interpolated Stan model
setGeneric("load_and_interpolate_stan_model", function(outcome, borrowing, analysis_obj) standardGeneric("load_and_interpolate_stan_model"))

# Survival ----
## Exponential ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvExponential", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    template <- load_stan_file("surv", "exp_nb.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;")
    )

    return(model_string)

  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeSurvExponential", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    template <- load_stan_file("surv", "exp_hcp.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;"),
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;")
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

    template <- load_stan_file("surv", "weib_ph_nb.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
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

    template <- load_stan_file("surv", "weib_ph_hcp.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      shape.prior = h_glue("shape_weibull ~ {{get_prior_string(outcome@param_priors$shape_weibull)}} ;"),
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

    template <- load_stan_file("bin", "logit_nb.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;")
    )

    return(model_string)

  }
)

### Hierarchical commensurate prior borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeBinaryLogistic", "BorrowingHierarchicalCommensurate", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    template <- load_stan_file("bin", "logit_hcp.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;")
    )

    return(model_string)

  }
)


# Continuous ----
## Logistic ----
### No/full borrowing ----
setMethod(
  f = "load_and_interpolate_stan_model",
  signature = c("OutcomeContinuousNormal", "BorrowingNoneFull", "ANY"),
  definition = function(outcome, borrowing, analysis_obj) {

    template <- load_stan_file("cont", "gauss_nb.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
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

    template <- load_stan_file("cont", "gauss_hcp.stan")

    model_string <- h_glue(
      template,
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weight;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha[2] ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
      tau.prior = h_glue("tau ~ {{get_prior_string(borrowing@tau_prior)}} ;"),
      stdev.prior = h_glue("std_dev_outcome ~ {{get_prior_string(outcome@param_priors$std_dev_outcome)}} ;")
    )

    return(model_string)

  }
)

