#' `PriorNormal` Class
#'
#' A class for defining normal priors to be translated to Stan code.
#' Objects of class `PriorNormal` should not be created directly but by
#' the constructor [prior_normal()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for normal stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' (all values allowed in normal distribution).
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family prior classes
.prior_normal <- setClass(
  "PriorNormal",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "normal({{object@mu}}, {{object@sigma}})",
    constraint = ""
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior normal distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/normal-distribution.html>
#'
#' @return Object of class [`PriorNormal`][PriorNormal-class].
#' @export
#' @family priors
#' @examples
#' np <- prior_normal(1, 1)
prior_normal <- function(mu, sigma) {
  .prior_normal(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "PriorNormal",
  definition = function(object) {
    cat("Normal Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("mu", "sigma"),
        R = c("mean", "sd"),
        Value = c(object@mu, object@sigma)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)

# plot ----
#' @rdname plot
#' @examples
#' plot(prior_normal(1, 2))
setMethod(
  f = "plot",
  signature = c("PriorNormal", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qnorm(c(0.005, 0.995), mean = x@mu, sd = x@sigma)
    density_fun <- function(values) stats::dnorm(values, mean = x@mu, sd = x@sigma)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)



#' Legacy function for the normal prior
#'
#' Please use `prior_normal()` instead.
#' @param ... Deprecated arguments to `normal_prior()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `normal_prior()` is deprecated and that 
#' `prior_normal()` should be used instead. 
#' 
#' @export
normal_prior <- function(...) {
  .Defunct(
    "prior_normal",
    "psborrow2",
    "`normal_prior()` is deprecated. Use `prior_normal()` instead."
  )
}


#' Legacy function for the normal half prior
#'
#' Please use `prior_half_normal()` instead.
#' @param ... Deprecated arguments to `half_normal_prior()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `half_normal_prior()` is deprecated and that 
#' `prior_half_normal()` should be used instead. 
#' 
#' @export
half_normal_prior <- function(...) {
  .Defunct(
    "prior_half_normal",
    "psborrow2",
    "`half_normal_prior()` is deprecated. Use `prior_half_normal()` instead."
  )
}
