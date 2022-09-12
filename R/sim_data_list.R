.sim_data_list <- setClass(
  "SimDataList",
  slots = c(guide = "data.frame",
            data_list = "list",
            effect = "character",
            drift = "character"
  ),
  validity = function(object) {
    if (NROW(object@guide) != NROW(object@data_list)) {
      return("`guide` and `data_list` must be same length")
    }
  }
)

sim_data_list <- function(guide,
                          data_list,
                          effect,
                          drift){

  .sim_data_list(
    guide = guide,
    data_list = data_list,
    effect = effect,
    drift = drift
  )

}
