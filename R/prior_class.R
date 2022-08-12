#' `Prior` Class
#'
#' A class for defining priors to be translated to Stan code. Objects of class
#' `Prior` should not be created directly but by one of the specific prior
#' class constructors.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for parameters surrounded with `{{` and `}}` to be replaced
#' with [glue::glue()].
#' @slot n_param integer. Number of prior parameters.
#' @slot constraint character. Support of prior distribution expressed as a
#' Stan constraint, e.g. `"<lower=0, upper=1>"`.
#' @family priors
#' @exportClass Prior
setClass(
  "Prior",
  slots = c(
    stan_code = "character",
    n_param = "integer",
    constraint = "character"
  ),
  validity = function(object) {
    check_string(object@stan_code)
    check_integer(object@n_param, lower = 1, len = 1, any.missing = FALSE)
    check_string(object@constraint)
  },
  contains = "VIRTUAL"
)

# show ----
setMethod(
  f = "show",
  signature = "Prior",
  definition = function(object) {
    cat(
      class(object)[1],
      "object with parameters",
      glue::glue(object@stan_code, .open = "{{", .close = "}}")
    )
  }
)
