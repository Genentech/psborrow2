#' `BorrowingCaseWeights` class
#'
#' @slot method_name string. The name of the method.
#' @slot ext_flag_col character. Name of the external flag column in the matrix.
#' @slot p
#' @slot q
#' @slot c
#'
#' @include borrowing_class.R
#' @family borrowing classes
.borrowing_case_weights <- setClass(
  "BorrowingCaseWeights",
  slots = c(
    p = "numeric",
    q = "numeric",
    c = "numeric",
    samples = "numeric"
  ),
  prototype = list(
    method_name = "Borrowing with case weights power prior"
  ),
  contains = "Borrowing",
  validity = function(object) {
    # check bounds on p, q, c?
    return(TRUE)
  }
)

#' Case Weights Power Prior borrowing
#'
#' @param ext_flag_col character. Name of the external flag column in the matrix.
#' @param q The scale parameter in the global transform
#' @param c The shift parameter in the global transform
#' @param p The power used in the polynomial transform.
#' @param samples A numeric vector with 2 values: the number of samples for all observations, the number of samples of
#'
#'
#' @details
#'
#' ## Method
#' For details on the transformation parameters, see equation 9 and 10 in the supplementary material of
#'  Kwiatkowski et al.
#'
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#'
#'
#' @references
#'
#' Kwiatkowski, E., Zhu, J., Li, X., Pang, H., Lieberman, G., & Psioda, M. A. (2024).
#'  Case weighted power priors for hybrid control analyses with time-to-event data.
#'   __Biometrics, 80(2), ujae019__. \doi{10.1093/biomtc/ujae019}
#'
#' @return Object of class [`BorrowingCaseWeights`][BorrowingCaseWeights-class].
#' @export
#' @examples
#' borrowing_case_weights(
#'   ext_flag_col = "ext",
#'   p = 1,
#'   q = 50,
#'   c = 0,
#'   samples = c(100, 100)
#' )
borrowing_case_weights <- function(ext_flag_col, p = 1, c = 0, q = 500, samples = c(100, 100)) {
  assert_string(ext_flag_col)
  .borrowing_case_weights(ext_flag_col = ext_flag_col, p = p, c = c, q = q, samples = samples)
}

# show ----
setMethod(
  f = "show",
  signature = "BorrowingCaseWeights",
  definition = function(object) {
    callNextMethod()
    cat("Case Weight Transformations:\n")
    show(object@c)
    show(object@q)
    show(object@p)
  }
)

# trim cols ----
#' @rdname trim_cols
#' @include generics.R
setMethod(
  f = "trim_cols",
  signature = "BorrowingCaseWeights",
  definition = function(borrowing_object, analysis_object) {
    return(get_vars(analysis_object))
  }
)
