#' Specify outcome details
#'
#' Specify the outcome model for time to event or binary endpoints.
#'
#' @param outcome_obj A `TimeToEvent` or `BinaryEndpoint` object
#'
#' @rdname set_outcome
#' @return An object inheriting from `Outcome` class
#' @export
#'
setGeneric("set_outcome", function(outcome_obj,
                                   ...) {
  if (!class(outcome_obj) %in% c(
    "ExponentialSurvDist",
    "WeibullPHSurvDist",
    "LogisticBinaryEndpoint"
  )) {
    stop("outcome_obj must be a time to event or binary outcome object")
  }
  standardGeneric("set_outcome")
})


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


#' @param endpoint_var Name of binary (1/0 or T/F) endpoint variable in the
#' model matrix
#'
#' @export
#' @rdname set_outcome
#' @examples
#'
#' # For binary outcomes
#' oo <- set_outcome(logistic_bin_endpoint(), "outcome")
#'
setMethod(
  "set_outcome",
  c(outcome_obj = "BinaryEndpoint"),
  function(outcome_obj,
           endpoint_var) {
    outcome_obj@endpoint_var <- endpoint_var
    outcome_obj
  }
)
