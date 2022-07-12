#' @include covariate_class.R
#' @include outcome_class.R
#' @include borrowing_class.R
#' @include treatment_class.R
#' @import cmdstanr

# Set analysis class union
setClassUnion("CovariatesOrNULL", c("Covariates","NULL"))

# Parent class
.analysis_obj <- setClass(
   "Analysis",
   slots = c(   model_matrix = "matrix",
                covariates = "CovariatesOrNULL",
                outcome = "Outcome",
                borrowing = "Borrowing",
                treatment_arms = "Treatment",
                model_and_data = "list",
                ready_to_sample = "logical"),
   prototype = list(
      ready_to_sample = FALSE
   )
)

# Print method
setMethod(
   f = "show",
   signature = "Analysis",
   definition = function(object) {
      if(!object@ready_to_sample) {
         cat("Analysis object (not ready to sample)")
      }
      if(object@ready_to_sample) {
         cat("Analysis object (compiled and ready to sample)",
             "Call mcmc_sample() next.")
      }
   }
)
