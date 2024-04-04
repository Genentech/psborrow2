#' `PriorCauchy` Class
#'
#' A class for defining the cauchy priors to be translated to Stan code.
#' Objects of class `PriorCauchy` should not be created directly but by
#' the constructor [prior_cauchy()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for cauchy stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution,
#' (all values allowed in cauchy distribution).
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family prior classes
.prior_cauchy <- setClass(
  "PriorCauchy",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "cauchy({{object@mu}}, {{object@sigma}})",
    constraint = ""
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior cauchy distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/cauchy-distribution.html>
#'
#' @return Object of class [`PriorCauchy`][PriorCauchy-class].
#' @export
#' @family priors
#' @examples
#' cp <- prior_cauchy(1, 1)
prior_cauchy <- function(mu, sigma) {
  .prior_cauchy(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "PriorCauchy",
  definition = function(object) {
    cat("Cauchy Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("mu", "sigma"),
        R = c("location", "scale"),
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
#' plot(prior_cauchy(0, 1), xlim = c(-20, 20))
#' plot(prior_cauchy(0, 2), xlim = c(-20, 20), col = 2, add = TRUE)
setMethod(
  f = "plot",
  signature = c("PriorCauchy", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qcauchy(c(0.005, 0.995), location = x@mu, scale = x@sigma)
    density_fun <- function(values) stats::dcauchy(values, location = x@mu, scale = x@sigma)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)



#' Legacy function for the cauchy prior
#'
#' Please use `prior_cauchy()` instead.
#' @param ... Deprecated arguments to `cauchy_prior()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `cauchy_prior()` is deprecated and that 
#' `prior_cauchy()` should be used instead.
#' 
#' @export
cauchy_prior <- function(...) {
  .Defunct(
    "prior_cauchy",
    "psborrow2",
    "`cauchy_prior()` is deprecated. Use `prior_cauchy()` instead."
  )
}
