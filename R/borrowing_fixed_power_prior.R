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
    power_par = "numeric"
  ),
  prototype = list(
    method_name = "Borrowing with fixed power prior"
  ),
  contains = "Borrowing"
)

#' Fixed Power Prior Borrowing
#'
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#' @param power_par numeric. Fixed power parameter for all external data. Must be between 0 and 1.
#'
#' @return Object of class [`BorrowingFixedPowerPrior`][BorrowingFixedPowerPrior-class].
#' @export
#' @examples
#' borrowing_fixed_power_prior(
#'   ext_flag_col = "ext",
#'   power_par = 0.5
#' )
borrowing_fixed_power_prior <- function(ext_flag_col, power_par) {
  assert_string(ext_flag_col)
  assert_number(power_par, lower = 0, upper = 1)
  .borrowing_fixed_power_prior(ext_flag_col = ext_flag_col, power_par = power_par)
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
    get_vars(analysis_object)
  }
)
# get vars -----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "BorrowingFixedPowerPrior",
  definition = function(object) {
    c(ext_flag_col = object@ext_flag_col)
  }
)
