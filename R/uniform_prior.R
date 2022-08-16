#' `UniformPrior` Class
#'
#' A class for defining uniform priors to be translated to Stan code.
#' Objects of class `UniformPrior` should not be created directly but by
#' the constructor `uniform_prior()`.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for uniform stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' `"<lower=`alpha`, upper=`beta`>"`.
#' @slot alpha numeric. Lower bound.
#' @slot beta numeric. Upper bound (>`alpha`).
#' @include prior_class.R
#' @family priors
.uniform_prior <- setClass(
  "UniformPrior",
  contains = "Prior",
  slots = c(
    alpha = "numeric",
    beta = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "uniform({{object@alpha}}, {{object@beta}})",
    constraint = "<lower={{object@alpha}}, upper = {{object@beta}}>"
  ),
  validity = function(object) {
    if (object@beta <= object@alpha) {
      return("beta must be > alpha")
    }
    return(TRUE)
  }
)

#' Prior uniform distribution
#'
#' @param alpha numeric. Lower bound.
#' @param beta numeric. Upper bound (>`alpha`).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/uniform-distribution.html>
#'
#' @return object of class "UniformPrior"
#' @export
#' @family priors
#' @examples
#' up <- uniform_prior(0, 1)
uniform_prior <- function(alpha, beta) {
  constraint <- glue::glue("<lower={{alpha}}, upper={{beta}}>", .open = "{{", .close = "}}")
  .uniform_prior(alpha = alpha, beta = beta, constraint = constraint)
}
