#' `BorrowingFixedPowerPrior` class
#'
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot powers numeric. The fixed values to be used as the powers in the power prior.
#'
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_fixed_power_prior <- setClass(
  "BorrowingFixedPowerPrior",
  slots = c(
    power_col = "character"
  ),
  prototype = list(
    method_name = "Borrowing with fixed power prior"
  ),
  contains = "Borrowing"
)

#' Fixed Power Prior Borrowing
#'
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#' @param power_col character. Name of the column containing values to be used as the power parameters.
#'
#' @return Object of class [`BorrowingFixedPowerPrior`][BorrowingFixedPowerPrior-class].
#' @export
#' @examples
#' borrowing_fixed_power_prior(
#'   ext_flag_col = "ext",
#'   power_col = "power")
#' )
borrowing_fixed_power_prior <- function(ext_flag_col, power_col) {
  assert_string(ext_flag_col)
  assert_string(power_col)
  .borrowing_fixed_power_prior(ext_flag_col = ext_flag_col, power_col = power_col)
}

# show ----
setMethod(
  f = "show",
  signature = "BorrowingFixedPowerPrior",
  definition = function(object) {
    callNextMethod()
  }
)

# trim cols ----
#' @rdname trim_cols
#' @include generics.R
setMethod(
  f = "trim_cols",
  signature = "BorrowingFixedPowerPrior",
  definition = function(borrowing_object, analysis_object) {
    c(ext_flag_col = object@ext_flag_col, power_col = power_col)
  }
)
