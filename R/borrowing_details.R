#' Legacy function for specifying borrowing details
#' 
#' Please use one of `hierarchical_borrowing()`, `no_borrowing()`, or `full_borrowing()` instead.
#' @export
#' @param ... Deprecated arguments to `borrowing_details`.
#' @export
borrowing_details <- function(...) {

  .Defunct(
    "hierarchical_borrowing()",
    "psborrow2",
    "`borrowing_details()` is deprecated. Use `hierarchical_borrowing()` for dynamic borrowing, else `no_` or `full_borrowing()`."
  )
}
