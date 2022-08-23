#' `CauchyPrior` Class
#'
#' A class for defining the cauchy priors to be translated to Stan code.
#' Objects of class `CauchyPrior` should not be created directly but by
#' the constructor [cauchy_prior()].
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
.cauchy_prior <- setClass(
  "CauchyPrior",
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
#' @return Object of class [`CauchyPrior`][CauchyPrior-class].
#' @export
#' @family priors
#' @examples
#' cp <- cauchy_prior(1, 1)
cauchy_prior <- function(mu, sigma) {
  .cauchy_prior(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "CauchyPrior",
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
#' plot(cauchy_prior(0, 1), xlim = c(-20, 20))
#' plot(cauchy_prior(0, 2), xlim = c(-20, 20), col = 2, add = TRUE)
setMethod(
  f = "plot",
  signature = c("CauchyPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qcauchy(c(0.005, 0.995), location = x@mu, scale = x@sigma)
    density_fun <- function(values) stats::dcauchy(values, location = x@mu, scale = x@sigma)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
