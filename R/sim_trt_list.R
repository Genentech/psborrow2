.sim_trt_list <- setClass(
  "SimTreatmentList",
  slots = c(trt_list = "list",
            guide = "data.frame"),
  validity = function(object) {
    if (!all(vapply(object@trt_list,
                    function(b) is(b, "Treatment"),
                    FUN.VALUE = logical(1)))) {
      return("`trt_list` must be a list of `Treatment` objects.")
    }
  }
)

sim_trt_list <- function(trt_list){

  trt <- .sim_trt_list(
    trt_list = trt_list
  )

  # Come up with nice print method at the covariate class level
  trt@guide <- data.frame(
    treatment_scenario = rep(1:NROW(trt_list))
  )

  trt


}
