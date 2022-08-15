#' @include prior_class.R

# Internal constructor
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
#' @param alpha lower bound
#' @param beta upper bound (>`alpha`)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/uniform-distribution.html>
#'
#' @return object of class `UniformPrior`
#' @export
#' @family priors
#' @examples
#' up <- uniform_prior(0, 1)
uniform_prior <- function(alpha, beta) {
  constraint <- glue::glue("<lower={{alpha}}, upper={{beta}}>", .open = "{{", .close = "}}")
  .uniform_prior(alpha = alpha, beta = beta, constraint = constraint)
}


# summary ----
setMethod(
  f = "summary",
  signature = "UniformPrior",
  definition = function(object) {
    show(object)

    x <- seq(from = object@alpha, to = object@beta, length = 2)
    y <- stats::dunif(x, min = object@alpha, max = object@beta)
    plot_pdf(x, y)
  }
)
