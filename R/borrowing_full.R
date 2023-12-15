#' `BorrowingFull` class
#' 
#' A class for defining details for "Full Borowing" methods. 
#' Objects of class `BorrowingFull`
#' should not be created directly but by the constructor
#' [borrowing_full()].
#' 
#' @slot data_stan_code string. Stan code that will be interpolated into the model.
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_full <- setClass(
   "BorrowingFull",
   prototype = list(
      data_stan_code = ""
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
#' This method does not distinguish between internal and external arms, effectively 
#' pooling patients.
#' 
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#' 
#' @return Object of class [`BorrowingFull`][BorrowingFull-class].
#' @include borrowing_class.R
#' 
#' @examples
#' db <- borrowing_full()
borrowing_full <- function() {
   .borrowing_full()
}

# show ----
setMethod(
  f = "show",
  signature = "BorrowingFull",
  definition = function(object) {
    cat("Borrowing object using the `full borrowing` approach\n\n")
   }
)

# get_vars ----
setMethod(
  f = "get_vars",
  signature = "BorrowingFull",
  definition = function(object) {
    c()
  }
)