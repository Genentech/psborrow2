#' Legacy function for specifying borrowing details
#'
#' Please use one of `borrowing_hierarchical_commensurate()`, `borrowing_none()`, or `borrowing_full()` instead.
#' @export
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `borrowing_details()` is deprecated and that 
#' one of `borrowing_hierarchical_commensurate()`, `borrowing_none()`, or
#' `borrowing_full()` should be used instead.
#' 
#' @param ... Deprecated arguments to `borrowing_details`.
borrowing_details <- function(...) {
  .Defunct(
    "borrowing_hierarchical_commensurate()",
    "psborrow2",
    paste(
      "`borrowing_details()` is deprecated. Use `borrowing_hierarchical_commensurate()` for dynamic borrowing,",
      "else `borrowing_none()` or `borrowing_full()`."
    )
  )
}
