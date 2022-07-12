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
                model = "environment")
)

# Print method
setMethod(
   f = "show",
   signature = "Analysis",
   definition = function(object) {
      cat("Analysis object")
   }
)
