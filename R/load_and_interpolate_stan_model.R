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
    return(template)
  }
)