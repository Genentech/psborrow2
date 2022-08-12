#' @include prior_class.R

setClassUnion("PriorOrNULL", c("Prior", "NULL"))

# borrowing class ----
.borrowing_class <- setClass(
  "Borrowing",
  slots = c(
    method = "character",
    ext_flag_col = "character",
    tau_prior = "PriorOrNULL",
    baseline_prior = "Prior"
  ),
  prototype = c(
    method = "No borrowing",
    ext_flag_col = NULL,
    tau_prior = NULL,
    baseline_prior = NULL
  ),
  validity = function(object) {
    assert_choice(object@method, c("Full borrowing", "No borrowing", "BDB"))
    if (object@method != "BDB" && !is.null(object@tau_prior)) {
      return("no need to specify tau prior when method is not BDB")
    }

    return(TRUE)
  }
)

# show ----
setMethod(
  f = "show",
  signature = "Borrowing",
  definition = function(object) {
    cat(
      "Borrowing class: ",
      object@method
    )
  }
)
