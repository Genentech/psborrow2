#' `ExponentialPrior` Class
#'
#' A class for defining exponential priors to be translated to Stan code.
#' Objects of class `ExponentialPrior` should not be created directly but by
#' the constructor `exponential_prior()`.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for exponential Stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (1).
#' @slot constraint character. Support of prior distribution,
#' `"<lower=0>"`.
#' @slot beta numeric. Inverse scale (>0).
#' @include prior_class.R
#' @family priors
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
#' @param beta numeric. Inverse scale (>0).
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

# show ----
setMethod(
  f = "show",
  signature = "ExponentialPrior",
  definition = function(object) {
    cat("Exponential Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("beta"),
        R = c("rate"),
        Value = c(object@beta)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)

# plot ----
#' @rdname plot
#' @examples
#' plot(exponential_prior(0.1))
setMethod(
  f = "plot",
  signature = c("ExponentialPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qexp(c(0.005, 0.995), rate = x@beta)
    density_fun <- function(values) stats::dexp(values, rate = x@beta)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
