#' `SimBorrowingList` Class
#'
#' A class for borrowing details as part of a simulation study.
#'  Objects of class `SimBorrowingList` should not be created
#' directly but by the constructor `sim_borrowing_list()`.
#'
#' @slot borrowing_list named list of object of class `Borrowing`, one object
#' for each parameter variation.
#'
#' @include borrowing_class.R
#' @include borrowing_full.R
#' @include borrowing_none.R
#' @include borrowing_hierarchical_commensurate.R
.sim_borrowing_list <- setClass(
  "SimBorrowingList",
  slots = c(
    borrowing_list = "list",
    guide = "data.frame"
  ),
  validity = function(object) {
    if (!all(vapply(object@borrowing_list,
      function(item) is(item, "Borrowing"),
      FUN.VALUE = logical(1)
    ))) {
      return("`borrowing_list` must be a list of `Borrowing` objects.")
    }
    if (is.null(names(object@borrowing_list))) {
      return("`borrowing_list` must be named.")
    }
    if (any(names(object@borrowing_list) == "")) {
      return("All items in `borrowing_list` must be named.")
    }
    if (length(unique(names(object@borrowing_list))) != length(names(object@borrowing_list))) {
      return("All names supplied to `borrowing_list` must be unique.")
    }
  }
)


#' Input borrowing details for a simulation study
#'
#' A function for defining which borrowing scenarios should be evaluated as
#' part of a simulation study.
#'
#' @param borrowing_list named list of objects of class `Borrowing` created
#' by `borrowing_full()`, `borrowing_none()`, or `borrowing_hierarchical_commensurate()`.
#'
#' @return Object of class [`SimBorrowingList`][SimBorrowingList-class].
#' 
#' @export
#'
#' @family simulation classes
#'
#' @examples
#'
#' borrow_scenarios <- sim_borrowing_list(
#'   list(
#'     "No borrowing" = borrowing_none("ext"),
#'     "Full borrowing" = borrowing_full("ext"),
#'     "BDB, uninformative prior" = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_gamma(0.001, 0.001)
#'     ),
#'     "BDB, informative prior" = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_gamma(1, 0.001)
#'     )
#'   )
#' )
#'
sim_borrowing_list <- function(borrowing_list) {
  borrow <- .sim_borrowing_list(
    borrowing_list = borrowing_list
  )

  borrow@guide <- data.frame(
    borrowing_scenario = names(borrow@borrowing_list)
  )

  borrow
}

# show ----
setMethod(
  f = "show",
  signature = "SimBorrowingList",
  definition = function(object) {
    cat("SimBorrowingList object with ", NROW(object@borrowing_list), " different scenario(s)\n")
    if (NROW(object@borrowing_list) <= 10) {
      print(object@guide)
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "SimBorrowingList",
  definition = function(object) {
    unique(vapply(object@borrowing_list, get_vars, character(1)))
  }
)
