#' `PriorBernoulli` Class
#'
#' A class for defining bernoulli priors to be translated to Stan code.
#' Objects of class `PriorBernoulli` should not be created directly but by
#' the constructor [prior_bernoulli()].
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
.prior_bernoulli <- setClass(
  "PriorBernoulli",
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
#' @return Object of class [`PriorBernoulli`][PriorBernoulli-class].
#' @export
#' @family priors
#' @examples
#' bp <- prior_bernoulli(0.23)
prior_bernoulli <- function(theta) {
  .prior_bernoulli(theta = theta)
}

# show ----
setMethod(
  f = "show",
  signature = "PriorBernoulli",
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
#' plot(prior_bernoulli(0.4), xlim = c(0, 15))
setMethod(
  f = "plot",
  signature = c("PriorBernoulli", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- c(0, 1)
    density_fun <- function(values) stats::dbinom(values, prob = x@theta, size = 1)
    dist_type <- "discrete"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)

#' Legacy function for the bernoulli prior
#'
#' Please use `prior_bernoulli()` instead.
#' @param ... Deprecated arguments to `bernoulli_prior()`.
#'
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `bernoulli_prior()` is deprecated and that 
#' `prior_bernoulli()` should be used instead.
#' 
#' @export
bernoulli_prior <- function(...) {
  .Defunct(
    "prior_bernoulli",
    "psborrow2",
    "`bernoulli_prior()` is deprecated. Use `prior_bernoulli()` instead."
  )
}
