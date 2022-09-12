.sim_borrowing_list <- setClass(
  "SimBorrowList",
  slots = c(borrow_list = "list",
            guide = "data.frame"),
  validity = function(object) {
    if (!all(vapply(object@borrow_list,
                   function(b) is(b, "Borrowing"),
                   FUN.VALUE = logical(1)))) {
      return("`borrow_list` must be a list of `Borrowing` objects.")
    }
  }
)

sim_borrowing_list <- function(borrow_list){

  borrow <- .sim_borrowing_list(
    borrow_list = borrow_list
  )

  # Come up with nice print method at the borrowing class level
  borrow@guide <- data.frame(
    borrowing_scenario = rep(1:NROW(borrow_list))
  )

  borrow

}
