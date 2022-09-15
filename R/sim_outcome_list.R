#' `SimOutcomeList` Class
#'
#' A class for outcome details as part of a simulation study.
#' Objects of class `SimOutcomeList` should not be created
#' directly but by the constructor `sim_outcome_list()`.
#'
#' @slot outcome_list named list of object of class `Outcome`, one object
#' for each parameter variation.
#'
#' @include outcome_class.R
#' @include exp_surv_dist.R
#' @include weib_ph_surv_dist.R
#' @include logistic_bin_outcome.R
.sim_outcome_list <- setClass(
  "SimOutcomeList",
  slots = c(
    outcome_list = "list",
    guide = "data.frame"
  ),
  validity = function(object) {
    if (!all(vapply(object@outcome_list,
      function(item) is(item, "Outcome"),
      FUN.VALUE = logical(1)
    ))) {
      return("`outcome_list` must be a list of `Outcome` objects.")
    }
    if (is.null(names(object@outcome_list))) {
      return("`outcome_list` must be named.")
    }
    if (any(names(object@outcome_list) == "")) {
      return("All items in `outcome_list` must be named.")
    }
    if (length(unique(names(object@outcome_list))) != length(names(object@outcome_list))) {
      return("All names supplied to `outcome_list` must be unique.")
    }
  }
)

#' Input outcome details for a simulation study
#'
#' A function for defining which outcome scenarios should be evaluated as
#' part of a simulation study.
#'
#' @param outcome_list named list of objects of class `Outcome` created
#' by `outcome_details()`.
#'
#' @family simulation classes
#'
#' @export
#'
#' @examples
#'
#' outcome_scenarios <- sim_outcome_list(
#'   list(
#'     "Exponential" = exp_surv_dist("time", "cnsr", normal_prior(0, 10000))
#'   )
#' )
#'
sim_outcome_list <- function(outcome_list) {
  outcome <- .sim_outcome_list(
    outcome_list = outcome_list
  )

  outcome@guide <- data.frame(
    outcome_scenario = names(outcome@outcome_list)
  )

  outcome
}

# show ----
setMethod(
  f = "show",
  signature = "SimOutcomeList",
  definition = function(object) {
    cat("SimOutcomeList object with ", NROW(object@outcome_list), " different scenario(s)\n")
    if (NROW(object@outcome_list) <= 10) {
      print(object@guide)
    }
  }
)
