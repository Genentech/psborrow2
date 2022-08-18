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
#' @return object of class `PoissonPrior`
#' @export
#' @family priors
#' @examples
#' pp <- poisson_prior(100)
poisson_prior <- function(lambda) {
  .poisson_prior(lambda = lambda)
}

# show ----
setMethod(
  f = "show",
  signature = "PoissonPrior",
  definition = function(object) {
    cat("Poisson Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("lambda"),
        R = c("lambda"),
        Value = c(object@lambda)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)

# plot ----

#' @rdname plot
#' @examples
#' plot(poisson_prior(5), xlim = c(0, 15))
setMethod(
  f = "plot",
  signature = c("PoissonPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qpois(c(0.005, 0.995), lambda = x@lambda)
    density_fun <- function(values) stats::dpois(values, lambda = x@lambda)
    dist_type <- "discrete"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
