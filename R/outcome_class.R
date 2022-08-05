#' `Outcome` class
#'
#' @slot function_stan_code character.
#' @slot param_stan_code character.
#' @slot likelihood_stan_code character.
#' @slot n_param integer.
#' @slot param_priors list.
#'
#' @family outcome model
#' @rdname Outcome-class
setClass(
  "Outcome",
  contains = "VIRTUAL"
)


#' `TimeToEvent` class
#'
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#'
#' @rdname Outcome-class
#'
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
#'
#' @slot binary_var character. Variable used for outcome in `BinaryOutcome` objects.
#'
#' @rdname Outcome-class
#'
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

# Print method
setMethod(
  f = "show",
  signature = "Outcome",
  definition = function(object) {
    cat("Outcome object with class ", class(object)[1])
  }
)
