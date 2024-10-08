# class union ----
setClassUnion("BorrowingNoneFull", c("BorrowingFull", "BorrowingNone"))

#' Load and interpolate Stan model
#' @param object `Outcome` object
#' @param borrowing `Borrowing` object
#' @param analysis_obj `Analysis` object
#' @include outcome_class.R analysis_class.R borrowing_class.R covariate_class.R treatment_class.R load_stan_file.R
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
      weights.data = if (outcome@weight_var == "") "" else "vector[N] weights;",
      cov.data = if (!is.null(analysis_obj@covariates)) h_glue("int<lower=0> K;\n  matrix[N, K] X;\n  vector[K]  L_beta;\n  vector[K] U_beta;\n") else "",
      cov.parameters = if (!is.null(analysis_obj@covariates)) "vector<lower=L_beta, upper=U_beta>[K] beta;" else "",
      trt.prior = h_glue("beta_trt ~ {{get_prior_string(analysis_obj@treatment@trt_prior)}} ;"),
      cov.priors = if (!is.null(analysis_obj@covariates)) get_prior_string_covariates(analysis_obj@covariates) else "",
      cov.linpred = if (!is.null(analysis_obj@covariates)) "+ X * beta" else "",
      weights.likelihood = if (outcome@weight_var == "") "" else "* weight[i]",
      baseline.prior = h_glue("alpha ~ {{get_prior_string(outcome@baseline_prior)}} ;"),
    )

    return(model_string)

  }
)