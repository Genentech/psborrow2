#' @include prior_class.R

setClassUnion("PriorOrNULL", c("Prior", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))

# Parent class
.borrowing_class <- setClass(
  "Borrowing",
  slots = c(
    method = "character",
    ext_flag_col = "characterOrNULL",
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
    if (!object@method %in% c(
      "BDB",
      "Full borrowing",
      "No borrowing"
    )) {
      return("method must be within ('BDB', 'Full borrowing', 'No borrowing')")
    }
    if (object@method != "BDB" && !is.null(object@tau_prior)) {
      return("no need to specify tau prior when method is not BDB")
    }
    if (object@method != "BDB" && !is.null(object@ext_flag_col)) {
      return("no need to specify ext_flag_col prior when method is not BDB")
    }

    return(TRUE)
  }
)

# Print method
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
