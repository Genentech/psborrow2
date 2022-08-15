#' @include prior_class.R

# Internal constructor
.exponential_prior <- setClass(
  "ExponentialPrior",
  contains = "Prior",
  slots = c(beta = "numeric"),
  prototype = list(
    n_param = 1L,
    stan_code = "exponential({{object@beta}})",
    constraint = "<lower=0>"
  ),
  validity = function(object) {
    if (object@beta <= 0) {
      return("beta must be >0")
    }
    return(TRUE)
  }
)

#' Prior exponential distribution
#'
#' @param beta inverse scale (>0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/exponential-distribution.html>
#'
#' @return object of class `ExponentialPrior`
#' @export
#' @family priors
#' @examples
#' ep <- exponential_prior(1)
exponential_prior <- function(beta) {
  .exponential_prior(beta = beta)
}

# summary ----
setMethod(
  f = "summary",
  signature = "ExponentialPrior",
  definition = function(object) {
    show(object)

    xlim <- stats::qexp(c(0.005, 0.995), rate = object@beta)
    x <- seq(from = xlim[1], to = xlim[2], length = 300)
    y <- stats::dexp(x, rate = object@beta)
    plot_pdf(x, y)
  }
)
