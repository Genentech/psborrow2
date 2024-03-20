#' `BorrowingNone` class
#'
#' A class for defining details for "No borrowing" methods.
#' Objects of class `BorrowingNone`
#' should not be created directly but by the constructor
#' [borrowing_none()].
#'
#' @slot data_stan_code string. Code to include in the Stan data program block.
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_none <- setClass(
  "BorrowingNone",
  prototype = list(
    data_stan_code = "",
    method_name = "No borrowing"
  ),
  contains = "Borrowing"
)

#' No borrowing
#'
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#'
#' @details
#'
#' ## Method
#'
#' This method evaluates only the internal comparison,
#' ignoring historical controls. Note that this method will filter the
#' model matrix based on values in `ext_flag_col`.
#'
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#'
#' @return Object of class [`BorrowingNone`][BorrowingNone-class].
#' @family borrowing
#' @export
#' @examples
#' db <- borrowing_none(
#'   ext_flag_col = "ext"
#' )
borrowing_none <- function(ext_flag_col) {
  assert_string(ext_flag_col)
  .borrowing_none(ext_flag_col = ext_flag_col)
}

# trim rows ----
#' @rdname trim_rows
#' @include generics.R
setMethod(
  f = "trim_rows",
  signature = "BorrowingNone",
  definition = function(borrowing_object, analysis_object) {
    return(!as.logical(analysis_object@data_matrix[, get_vars(analysis_object@borrowing)]))
  }
)
