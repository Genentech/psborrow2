.sim_borrowing_list <- setClass(
  "SimBorrowList",
  slots = c(borrow_list = "list"),
  validity = function(object) {
    if (!all(vapply(object@borrow_list,
                   function(b) is(b, "Borrowing"),
                   FUN.VALUE = logical(1)))) {
      return("`borrow_list` must be a list of `Borrowing` objects.")
    }
  }
)

sim_borrowing_list <- function(borrow_list){

  .sim_borrowing_class(
    borrow_list = borrow_list
  )

}
