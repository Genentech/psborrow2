#' `NoBorrowing` class
#' 
#' A class for defining details for "No borrowing" methods. 
#' Objects of class `NoBorrowing`
#' should not be created directly but by the constructor
#' [no_borrowing()].
#' 
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot data_stan_code string. Stan code that will be interpolated into the model.
#' @include borrowing_class.R
#' @family borrowing classes
.no_borrowing <- setClass(
   "NoBorrowing",
   slots = c(
      ext_flag_col = "character",
      tau_prior = "Prior"
   ),
   prototype = list(
      data_stan_code = "" #@TODO update STAN code here
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
#' This method evaluates only the internal comparison,
#' ignoring historical controls. Note that this method will filter the
#' model matrix based on values in `ext_flag_col`.
#' 
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#' 
#' @return Object of class [`NoBorrowing`][NoBorrowing-class].
#' @include borrowing_class.R
#' 
#' @examples
#' db <- no_borrowing(
#'    ext_flag_col = "ext"
#' )
no_borrowing <- function(ext_flag_col) {
   .no_borrowing(ext_flag_col = ext_flag_col)
}

# show ----
setMethod(
  f = "show",
  signature = "NoBorrowing",
  definition = function(object) {
    cat("Borrowing object using the `no borrowing` approach\n\n")
    cat("External control flag:", object@ext_flag_col, "\n\n")
   }
)
