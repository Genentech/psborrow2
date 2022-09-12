.sim_borrowing_class <- setClass(
  "SimBorrowClass",
  slots = c(guide = "data.frame",
            data_list = "list"
  ),
  validity = function(object) {
    if (NROW(object@guide) != NROW(object@data_list)) {
      return("`guide` and `data_list` must be same length")
    }
  }
)

sim_orrowing_class <- function(guide,
                               data_list){

  .sim_data_list(
    guide = guide,
    data_list = data_list
  )

}
