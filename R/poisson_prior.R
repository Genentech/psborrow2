#' @include prior_class.R

# Internal constructor
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
#' @param lambda rate (>0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/poisson.html>
#'
#' @return object of class `PoissonPrior`
#' @export
#' @family priors
#' @examples
#' pp <- poisson_prior(100)
poisson_prior <- function(lambda) {
  .poisson_prior(lambda = lambda)
}

# summary ----
setMethod(
  f = "summary",
  signature = "PoissonPrior",
  definition = function(object) {
    show(object)

    xlim <- floor(stats::qpois(c(0.005, 0.995), lambda = object@lambda))
    x <- seq(xlim[1], xlim[2] + 1)
    y <- stats::dpois(x, lambda = object@lambda)
    plot_pmf(x, y)
  }
)
