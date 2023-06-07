#' `UniformPrior` Class
#'
#' A class for defining uniform priors to be translated to Stan code.
#' Objects of class `UniformPrior` should not be created directly but by
#' the constructor [uniform_prior()].
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
#' @family prior classes
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
    constraint = "<lower={{object@alpha}},upper={{object@beta}}>"
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
#' @return Object of class [`UniformPrior`][UniformPrior-class].
#' @export
#' @family priors
#' @examples
#' up <- uniform_prior(0, 1)
uniform_prior <- function(alpha, beta) {
  .uniform_prior(alpha = alpha, beta = beta)
}


# show ----
setMethod(
  f = "show",
  signature = "UniformPrior",
  definition = function(object) {
    cat("Uniform Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("alpha", "beta"),
        R = c("min", "max"),
        Value = c(object@alpha, object@beta)
      ),
      row.names = FALSE, right = FALSE
    )
    print(h_glue("Constraints: {{eval_constraints(object)}}"))
  }
)

# plot ----
#' @rdname plot
#' @examples
#' plot(uniform_prior(1, 2), xlim = c(0, 3))
setMethod(
  f = "plot",
  signature = c("UniformPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- c(x@alpha, x@beta)
    density_fun <- function(values) stats::dunif(values, min = x@alpha, max = x@beta)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
