#' @include prior_class.R
#' @include normal_prior.R

# Parent class
.treatment_class <- setClass(
   "Treatment",
   slots = c(ext_flag_col = "character",
             trt_flag_col = "character",
             trt_log_hazard_ratio_prior = "Prior",
             ext_log_hazard_rate_prior = "Prior"
   ),
   prototype = c(trt_log_hazard_ratio_prior = normal_prior(0,10000),
                 ext_log_hazard_rate_prior = normal_prior(0,10000)),
   validity = function(object) {
      return(TRUE)
   }
)

# Print method
setMethod(
   f = "show",
   signature = "Treatment",
   definition = function(object) {
      cat("Treatment class with external control flag column of `",
          object@ext_flag_col, "` and experimental treatment flag ",
          "column of `", object@trt_flag_col, "`")
   }
)
