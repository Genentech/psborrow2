#' @include prior_class.R
#' @include normal_prior.R

# Parent class
.treatment_class <- setClass(
   "Treatment",
   slots = c(trt_flag_col = "character",
             trt_log_hazard_ratio_prior = "Prior"
   ),
   prototype = c(trt_log_hazard_ratio_prior = normal_prior(0,10000)),
   validity = function(object) {
      return(TRUE)
   }
)

# Print method
setMethod(
   f = "show",
   signature = "Treatment",
   definition = function(object) {
      cat("Treatment class with experimental treatment flag ",
          "column of `", object@trt_flag_col, "`")
   }
)
