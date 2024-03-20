#' `Borrowing` Class
#'
#' A class for defining borrowing details. Objects of class
#' `Borrowing` should not be created directly but by the constructors
#' [borrowing_hierarchical_commensurate()], [borrowing_none()], [borrowing_full()].
#'
#' @slot data_stan_code string. Code to include in the Stan data program block.
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @family borrowing classes
#' @seealso Prior constructor functions: [borrowing_full()], [borrowing_hierarchical_commensurate()], [borrowing_none()]
setClass(
  "Borrowing",
  slots = c(
    data_stan_code = "character",
    method_name = "character",
    ext_flag_col = "character"
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

# trim rows ----
#' @rdname trim_rows
#' @include generics.R
setMethod(
  f = "trim_rows",
  signature = "Borrowing",
  definition = function(borrowing_object, analysis_object) {
    return(seq_len(NROW(analysis_object@data_matrix)))
  }
)

# trim cols ----
#' @rdname trim_cols
#' @include generics.R
setMethod(
  f = "trim_cols",
  signature = "Borrowing",
  definition = function(borrowing_object, analysis_object) {
    return(setdiff(get_vars(analysis_object), get_vars(analysis_object@borrowing)))
  }
)

# create alpha string ----
#' @rdname create_alpha_string
#' @include generics.R
setMethod(
  f = "create_alpha_string",
  signature = "Borrowing",
  definition = function(borrowing_object, outcome_object) {
    return(setNames("alpha", outcome_object@alpha_type))
  }
)

# create tau string ----
#' @rdname create_tau_string
#' @include generics.R
setMethod(
  f = "create_tau_string",
  signature = "Borrowing",
  definition = function(borrowing_object) {
    return(NULL)
  }
)
