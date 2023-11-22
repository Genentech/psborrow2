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
#' @include prior_normal.R
.treatment_class <- setClass(
  "Treatment",
  slots = c(
    trt_flag_col = "character",
    trt_prior = "Prior"
  ),
  prototype = c(trt_prior = prior_normal(0, 1000)),
  validity = function(object) {
    return(TRUE)
  }
)

# show ----
setMethod(
  f = "show",
  signature = "Treatment",
  definition = function(object) {
    cat("Treatment object\n\n")
    cat("Treatment flag column:", object@trt_flag_col, "\n\n")
    cat("Treatment effect prior:\n")
    show(object@trt_prior)
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
#' @examples
#' get_vars(treatment_details(
#'   trt_flag_col = "treat_fl",
#'   trt_prior = prior_normal(0, 1000)
#' ))
setMethod(
  f = "get_vars",
  signature = "Treatment",
  definition = function(object) {
    c(trt_flag_col = object@trt_flag_col)
  }
)
