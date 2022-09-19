# class union ----
setClassUnion("SimCovariateListOrNULL", c("SimCovariateList", "NULL"))

#' `Simulation` Class
#'
#' A class for defining Simulation study details. Objects of class
#' `Simulation` should not be created directly but by the constructor
#' `create_simulation_obj()`.
#'
#' @slot data_list `SimDataList`. The list of lists of data matrices created
#' with `sim_data_list()`.
#' @slot outcome_list `SimOutcomeList`. List of outcome objects created with
#' `sim_outcome_list()`.
#' @slot borrowing_list `SimBorrowingList`. List of borrowing objects created
#' with `sim_borrowing_list()`.
#' @slot covariate_list `SimCovariateList`. List of covariate objects created
#' with `sim_covariate_list()`.
#' @slot treatment_list `SimTreatmentList`. List of treatment objects created
#' with `sim_treatment_list()`.
#' @include sim_data_list.R
#' @include sim_covariate_list.R
#' @include sim_borrowing_list.R
#' @include sim_outcome_list.R
#' @include sim_treatment_list.R
.simulation_obj <- setClass(
  "Simulation",
  slots = c(
    data_list = "SimDataList",
    outcome_list = "SimOutcomeList",
    borrowing_list = "SimBorrowingList",
    covariate_list = "SimCovariateListOrNULL",
    treatment_list = "SimTreatmentList"
  )
)

# show ----
setMethod(
  f = "show",
  signature = "Simulation",
  definition = function(object) {
    cat("Simulation object")
  }
)
