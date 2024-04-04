#' `PriorGamma` Class
#'
#' A class for defining gamma priors to be translated to Stan code.
#' Objects of class `PriorGamma` should not be created directly but by
#' the constructor [prior_gamma()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for gamma stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' `"<lower=0>"`.
#' @slot alpha numeric. Shape (>0).
#' @slot beta numeric. Inverse scale (>=0).
#' @include prior_class.R
#' @family prior classes
.prior_gamma <- setClass(
  "PriorGamma",
  contains = "Prior",
  slots = c(
    alpha = "numeric",
    beta = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "gamma({{object@alpha}}, {{object@beta}})",
    constraint = "<lower=0>"
  ),
  validity = function(object) {
    if (object@alpha <= 0 || object@beta <= 0) {
      return("Both alpha and beta must be >= 0")
    }
    return(TRUE)
  }
)

#' Prior gamma distribution
#'
#' @param alpha numeric. Shape (>0).
#' @param beta numeric. Inverse scale (>=0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/gamma-distribution.html>
#'
#' @return Object of class [`PriorGamma`][PriorGamma-class].
#' @export
#' @family priors
#' @examples
#' gp <- prior_gamma(0.001, 0.001)
prior_gamma <- function(alpha, beta) {
  .prior_gamma(alpha = alpha, beta = beta)
}


# show ----
setMethod(
  f = "show",
  signature = "PriorGamma",
  definition = function(object) {
    cat("Gamma Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("alpha", "beta"),
        R = c("shape", "rate"),
        Value = c(object@alpha, object@beta)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)

# plot ----
#' @rdname plot
#' @examples
#' plot(prior_gamma(0.1, 0.1))
setMethod(
  f = "plot",
  signature = c("PriorGamma", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- c(0, stats::qgamma(0.99, shape = x@alpha, rate = x@beta))
    density_fun <- function(values) stats::dgamma(values, shape = x@alpha, rate = x@beta)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)

#' Legacy function for the gamma prior
#'
#' Please use `prior_gamma()` instead.
#' @param ... Deprecated arguments to `gamma_prior()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `gamma_prior()` is deprecated and that 
#' `prior_gamma()` should be used instead. 
#'
#' @export
gamma_prior <- function(...) {
  .Defunct(
    "prior_gamma",
    "psborrow2",
    "`gamma_prior()` is deprecated. Use `prior_gamma()` instead."
  )
}
