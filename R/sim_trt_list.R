.sim_trt_list <- setClass(
  "SimTreatmentList",
  slots = c(trt_list = "list"),
  validity = function(object) {
    if (!all(vapply(object@trt_list,
                    function(b) is(b, "Treatment"),
                    FUN.VALUE = logical(1)))) {
      return("`trt_list` must be a list of `Treatment` objects.")
    }
  }
)

sim_trt_list <- function(covariate_list){

  .sim_trt_list(
    trt_list = trt_list
  )

}
