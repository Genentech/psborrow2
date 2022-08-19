#' `BernoulliPrior` Class
#'
#' A class for defining bernoulli priors to be translated to Stan code.
#' Objects of class `BernoulliPrior` should not be created directly but by
#' the constructor [bernoulli_prior()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for bernoulli stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (1).
#' @slot constraint character. Support of prior distribution,
#' `"<lower=0, upper=1>"`.
#' @slot theta numeric. Probability (in \[0, 1\]).
#' @include prior_class.R
#' @family prior classes
.bernoulli_prior <- setClass(
  "BernoulliPrior",
  contains = "Prior",
  slots = c(theta = "numeric"),
  prototype = list(
    n_param = 1L,
    stan_code = "bernoulli({{object@theta}})",
    constraint = "<lower=0, upper=1>"
  ),
  validity = function(object) {
    if (object@theta < 0 || object@theta > 1) {
      return("theta must be within [0,1]")
    }
    return(TRUE)
  }
)

#' Prior bernoulli distribution
#'
#' @param theta numeric. Probability (in \[0, 1\]).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/bernoulli-distribution.html>
#'
#' @return Object of class [`BernoulliPrior`][BernoulliPrior-class].
#' @export
#' @family priors
#' @examples
#' bp <- bernoulli_prior(0.23)
bernoulli_prior <- function(theta) {
  .bernoulli_prior(theta = theta)
}

# show ----
setMethod(
  f = "show",
  signature = "BernoulliPrior",
  definition = function(object) {
    cat("Bernoulli Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = "theta",
        R = "prob",
        Value = object@theta
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)
# plot ----
#' @rdname plot
#' @examples
#' plot(bernoulli_prior(0.4), xlim = c(0, 15))
setMethod(
  f = "plot",
  signature = c("BernoulliPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- c(0, 1)
    density_fun <- function(values) stats::dbinom(values, prob = x@theta, size = 1)
    dist_type <- "discrete"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
