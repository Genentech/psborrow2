#' `CauchyPrior` Class
#'
#' A class for defining the cauchy priors to be translated to Stan code.
#' Objects of class `CauchyPrior` should not be created directly but by
#' the constructor `cauchy_prior()`.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for cauchy stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' (all values allowed in cauchy distribution).
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family priors
.cauchy_prior <- setClass(
  "CauchyPrior",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "cauchy({{object@mu}},{{object@sigma}})",
    constraint = ""
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior cauchy distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/cauchy-distribution.html>
#'
#' @return object of class "CauchyPrior"
#' @export
#' @family priors
#' @examples
#' cp <- cauchy_prior(1, 1)
cauchy_prior <- function(mu, sigma) {
  .cauchy_prior(mu = mu, sigma = sigma)
}
