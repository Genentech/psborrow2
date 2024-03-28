#' `SimTreatmentList` Class
#'
#' A class for treatment details as part of a simulation study.
#' Objects of class `SimTreatmentList` should not be created
#' directly but by the constructor `sim_treatment_list()`.
#'
#' @slot treatment_list named list of object of class `Treatment`, one object
#' for each parameter variation.
#'
#' @include treatment_class.R
#' @include treatment_details.R
.sim_treatment_list <- setClass(
  "SimTreatmentList",
  slots = c(
    treatment_list = "list",
    guide = "data.frame"
  ),
  validity = function(object) {
    if (!all(vapply(object@treatment_list,
      function(item) is(item, "Treatment"),
      FUN.VALUE = logical(1)
    ))) {
      return("`treatment_list` must be a list of `Treatment` objects.")
    }
    if (is.null(names(object@treatment_list))) {
      return("`treatment_list` must be named.")
    }
    if (any(names(object@treatment_list) == "")) {
      return("All items in `treatment_list` must be named.")
    }
    if (length(unique(names(object@treatment_list))) != length(names(object@treatment_list))) {
      return("All names supplied to `treatment_list` must be unique.")
    }
  }
)

#' Input treatment details for a simulation study
#'
#' A function for defining which treatment scenarios should be evaluated as
#' part of a simulation study.
#'
#' @param treatment_list named list of objects of class `Treatment` created
#' by `treatment_details()`.
#'
#' @family simulation classes
#'
#' @return Object of class [`SimTreatmentList`][SimTreatmentList-class].
#'
#' @export
#'
#' @examples
#'
#' treatment_scenarios <- sim_treatment_list(
#'   list(
#'     "Standard" = treatment_details("trt", prior_normal(0, 1000))
#'   )
#' )
#'
sim_treatment_list <- function(treatment_list) {
  treatments <- .sim_treatment_list(
    treatment_list = treatment_list
  )

  treatments@guide <- data.frame(
    treatment_scenario = names(treatments@treatment_list)
  )

  treatments
}

# show ----
setMethod(
  f = "show",
  signature = "SimTreatmentList",
  definition = function(object) {
    cat("SimTreatmentList object with ", NROW(object@treatment_list), " different scenario(s)\n")
    if (NROW(object@treatment_list) <= 10) {
      print(object@guide)
    }
  }
)

# get_vars ----
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "SimTreatmentList",
  definition = function(object) {
    unique(vapply(object@treatment_list, get_vars, character(1)))
  }
)
