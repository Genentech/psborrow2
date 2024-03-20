#' `BorrowingFull` class
#'
#' A class for defining details for "Full Borrowing" methods.
#' Objects of class `BorrowingFull`
#' should not be created directly but by the constructor
#' [borrowing_full()].
#'
#' @slot data_stan_code string. Code to include in the Stan data program block.
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot name_tau named vector for hierarchical commensurability parameter hyperprior.
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_full <- setClass(
  "BorrowingFull",
  prototype = list(
    data_stan_code = "",
    method_name = "Full borrowing"
  ),
  contains = "Borrowing"
)

#' Full borrowing
#'
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#'
#' @details
#'
#' ## Method
#'
#' This method does not distinguish between internal and external arms, effectively
#' pooling patients.
#'
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#'
#' @return Object of class [`BorrowingFull`][BorrowingFull-class].
#' @family borrowing
#' @export
#' @examples
#' fb <- borrowing_full("ext")
borrowing_full <- function(ext_flag_col) {
  .borrowing_full(ext_flag_col = ext_flag_col)
}
