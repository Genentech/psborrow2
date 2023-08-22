# class union ----
setClassUnion("PriorOrNULL", c("Prior", "NULL"))

#' `Borrowing` Class
#'
#' A class for defining borrowing details. Objects of class
#' `Borrowing` should not be created directly but by the constructor
#' [borrowing_details()].
#'
#' @slot method character. The type of borrowing to perform. It
#' must be one of: `'BDB_HCP'`, `'Full borrowing'`, or `'No borrowing'`. See `?borrowing_details` for
#' more information.
#' @slot ext_flag_col character. The name of the column in
#' the data matrix that corresponds to the external control flag (`1`/`0` or
#' `TRUE`/`FALSE`). This identifies a patient as belonging to the external
#' control cohort.
#' @slot tau_prior `Prior`. Object of class `Prior` defining the hyperprior on the
#' "commensurability parameter". See `?borrowing_details` for more information.
#' @slot data_stan_code string. Code to include in the Stan data program block.
.borrowing_class <- setClass(
  "Borrowing",
  slots = c(
    method = "character",
    ext_flag_col = "character",
    tau_prior = "PriorOrNULL",
    data_stan_code = "character"
  ),
  prototype = c(
    method = "No borrowing",
    ext_flag_col = NULL,
    tau_prior = NULL,
    data_stan_code = ""
  ),
  validity = function(object) {
    assert_choice(object@method, c("Full borrowing", "No borrowing", "BDB_HCP"))
    if (object@method != "BDB_HCP" && !is.null(object@tau_prior)) {
      return("no need to specify tau prior when method is not BDB_HCP")
    }
    assert_string(object@data_stan_code)
    return(TRUE)
  }
)

# show ----
setMethod(
  f = "show",
  signature = "Borrowing",
  definition = function(object) {
    cat("Borrowing object using", object@method, "\n\n")
    cat("External control flag:", object@ext_flag_col, "\n\n")
    if (!is.null(object@tau_prior)) {
      cat("Commensurability parameter prior:\n")
      show(object@tau_prior)
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "Borrowing",
  definition = function(object) {
    c(ext_flag_col = object@ext_flag_col)
  }
)
