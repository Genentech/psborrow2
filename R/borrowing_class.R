#' `Borrowing` Class
#'
#' A class for defining borrowing details. Objects of class
#' `Borrowing` should not be created directly but by the constructors
#' [hierarchical_commensurate_borrowing(), no_borrowing(), full_borrowing()].
#'
#' @slot data_stan_code string. Code to include in the Stan data program block.
#' @slot method_name string. The name of the method.
setClass(
  "Borrowing",
  slots = c(
    data_stan_code = "character",
    method_name = "character"
  ),
  contains = "VIRTUAL"
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

# show ----
setMethod(
  f = "show",
  signature = "Borrowing",
  definition = function(object) {
    cat("Borrowing object using the ", object@method_name, " approach\n\n")
    cat("External control flag:", object@ext_flag_col, "\n\n")
   }
)
