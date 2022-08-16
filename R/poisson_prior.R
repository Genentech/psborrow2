#' `PoissonPrior` Class
#'
#' A class for defining poisson priors to be translated to Stan code.
#' Objects of class `PoissonPrior` should not be created directly but by
#' the constructor `poisson_prior()`.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for poisson stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (1).
#' @slot constraint character. Support of prior distribution,
#' `"<lower=0>"`.
#' @slot lambda numeric. Rate (>0).
#' @include prior_class.R
#' @family priors
.poisson_prior <- setClass(
  "PoissonPrior",
  contains = "Prior",
  slots = c(lambda = "numeric"),
  prototype = list(
    n_param = 1L,
    stan_code = "poisson({{object@lambda}})",
    constraint = "<lower=0>"
  ),
  validity = function(object) {
    if (object@lambda <= 0) {
      return("lambda must be >0")
    }
    return(TRUE)
  }
)

#' Prior poisson distribution
#'
#' @param lambda numeric. Rate (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/poisson.html>
#'
#' @return object of class "PoissonPrior"
#' @export
#' @family priors
#' @examples
#' pp <- poisson_prior(100)
poisson_prior <- function(lambda) {
  .poisson_prior(lambda = lambda)
}
