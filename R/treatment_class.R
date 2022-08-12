#' @include prior_class.R

# Treatment class ----
.treatment_class <- setClass(
  "Treatment",
  slots = c(
    trt_flag_col = "character",
    trt_prior = "Prior"
  ),
  prototype = c(trt_prior = normal_prior(0, 1000)),
  validity = function(object) {
    return(TRUE)
  }
)

# show ----
setMethod(
  f = "show",
  signature = "Treatment",
  definition = function(object) {
    cat(
      "Treatment class with experimental treatment flag ",
      "column of `", object@trt_flag_col, "`"
    )
  }
)
