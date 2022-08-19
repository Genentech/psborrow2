#' `Treatment` Class
#'
#' A class for defining treatment details. Objects of class
#' `Treatment` should not be created directly but by the constructor
#' [treatment_details()].
#'
#' @slot trt_flag_col character. Character specifying the name of the column
#' in the model matrix that corresponds to the treatment flag
#' (`1`/`0` or `TRUE`/`FALSE`). This identifies patients as belonging
#' to the experimental treatment arm.
#' @slot trt_prior `Prior`. Object of class `Prior` specifying the
#' prior distribution of the log effect estimate (log hazard ratio for
#' time to event endpoints and log odds ratio for binary endpoints).
#' @include prior_class.R
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
