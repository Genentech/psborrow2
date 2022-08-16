#' `NormalPrior` Class
#'
#' A class for defining normal priors to be translated to Stan code.
#' Objects of class `NormalPrior` should not be created directly but by
#' the constructor `normal_prior()`.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for normal stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' (all values allowed in normal distribution).
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family priors
.normal_prior <- setClass(
  "NormalPrior",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "normal({{object@mu}}, {{object@sigma}})",
    constraint = ""
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior normal distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/normal-distribution.html>
#'
#' @return object of class "NormalPrior"
#' @export
#' @family priors
#' @examples
#' np <- normal_prior(1, 1)
normal_prior <- function(mu, sigma) {
  .normal_prior(mu = mu, sigma = sigma)
}
