#' Legacy function for specifying borrowing details
#'
#' Please use one of `hierarchical_commensurate_borrowing()`, `no_borrowing()`, or `full_borrowing()` instead.
#' @export
#' @param ... Deprecated arguments to `borrowing_details`.
borrowing_details <- function(...) {
  .Defunct(
    "hierarchical_commensurate_borrowing()",
    "psborrow2",
    "`borrowing_details()` is deprecated. Use `hierarchical_commensurate_borrowing()` for dynamic borrowing, else `no_borrowing()` or `full_borrowing()`."
  )
}
