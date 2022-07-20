#' Specify outcome details
#'
#' Specify the outcome model for time to event or binary outcomes
#'
#' @param outcome_obj A `TimeToEvent` or `BinaryOutcome` object
#' @param ... Passed to class specific methods.
#'
#' @rdname set_outcome
#' @return An object inheriting from [`Outcome`][Outcome-class] class
#' @export
#'
setGeneric("set_outcome", function(outcome_obj, ...) {
  standardGeneric("set_outcome")
})

setMethod(
  "set_outcome",
  c("outcome_obj" = "Outcome"),
  function(outcome_obj, ...) {
    stop("outcome_obj must be a `TimeToEvent` or `BinaryOutome` object")
  }
)

#' @param time_var Name of time variable column in model matrix
#' @param cens_var Name of the censorship variable flag in model matrix
#'
#' @export
#' @rdname set_outcome
#' @examples
#'
#' # For time to event
#' oo <- set_outcome(exp_surv_dist(), "time_months", "cens_flag")
#'
setMethod(
  "set_outcome",
  c(outcome_obj = "TimeToEvent"),
  function(outcome_obj,
           time_var,
           cens_var) {
    outcome_obj@time_var <- time_var
    outcome_obj@cens_var <- cens_var
    outcome_obj
  }
)


#' @param binary_var Name of binary (1/0 or T/F) outcome variable in the
#' model matrix
#'
#' @export
#' @rdname set_outcome
#' @examples
#'
#' # For binary outcomes
#' oo <- set_outcome(logistic_bin_outcome(), "outcome")
#'
setMethod(
  "set_outcome",
  c(outcome_obj = "BinaryOutcome"),
  function(outcome_obj,
           binary_var) {
    outcome_obj@binary_var <- binary_var
    outcome_obj
  }
)
