#' @include sim_data_list.R
#' @include sim_borrowing_list.R
#' @include sim_outcome_list.R
#' @include sim_cov_list.R
#' @include sim_trt_list.R
NULL

.analysis_list <- setClass(
  "SimAnalysisList",
  slots = c(analysis_objects = "list"),
  validity =  function(object) {
    if (!all(vapply(object@outcome_list,
                    function(b) is(b, "Analysis"),
                    FUN.VALUE = logical(1)))) {
      return("`analysis_objects` must be a list of `Analysis` objects.")
    }
  }
)

setClassUnion('SimCovariateListOrNULL', c("SimCovariateList", "NULL"))

.create_simulation_obj <- setClass(
  "Simulation",
  slots = c(data_list = "SimDataList",
            outcome_list = "SimOutcomeList",
            borrowing_list = "SimBorrowList",
            cov_list = "SimCovariateListOrNULL",
            trt_list = "SimTreatmentList",
            analysis_list = "SimAnalysisList")
)

create_simulation_obj <- function(
    data_list,
    outcome_list,
    borrowing_list,
    cov_list,
    trt_list
    ){

  sim_obj <- .create_simulation_obj(
    data_list = data_list,
    outcome_list = outcome_list,
    borrowing_list = borrowing_list,
    cov_list = cov_list,
    trt_list = trt_list
  )

  # Crude way of getting all analysis objects

}
