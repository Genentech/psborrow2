#' Legacy function for specifying borrowing details
#'
#' Please use one of `hierarchical_commensurate_borrowing()`, `no_borrowing()`, or `full_borrowing()` instead.
#' @export
#' @param ... Deprecated arguments to `borrowing_details`.
borrowing_details <- function(...) {
  .Defunct(
    "hierarchical_commensurate_borrowing()",
    "psborrow2",
    paste(
      "`borrowing_details()` is deprecated. Use `borrowing_hierarchical_commensurate()` for dynamic borrowing,",
      "else `borrowing_none()` or `borrowing_full()`."
    )
  )
}
