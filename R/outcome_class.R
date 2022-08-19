#' `Outcome` class
#' @family outcome
setClass(
  "Outcome",
  contains = "VIRTUAL"
)

#' `TimeToEvent` class
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate.
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @family outcome
setClass(
  "TimeToEvent",
  slots = c(
    function_stan_code = "character",
    param_stan_code = "character",
    likelihood_stan_code = "character",
    n_param = "integer",
    param_priors = "list",
    time_var = "character",
    cens_var = "character"
  ),
  prototype = list(
    n_param = 0L,
    function_stan_code = "",
    param_stan_code = "",
    likelihood_stan_code = ""
  ),
  contains = "Outcome"
)

#' `BinaryOutcome` class
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate.
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot binary_var character. Variable used for outcome in `BinaryOutcome` objects.
#' @family outcome
setClass(
  "BinaryOutcome",
  slots = c(
    function_stan_code = "character",
    param_stan_code = "character",
    likelihood_stan_code = "character",
    n_param = "integer",
    param_priors = "list",
    binary_var = "character"
  ),
  prototype = list(
    n_param = 0L,
    function_stan_code = "",
    param_stan_code = "",
    likelihood_stan_code = ""
  ),
  contains = "Outcome"
)

# show ----
setMethod(
  f = "show",
  signature = "Outcome",
  definition = function(object) {
    cat("Outcome object with class ", class(object)[1])
  }
)
