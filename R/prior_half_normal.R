#' `PriorHalfNormal` Class
#'
#' A class for defining half normal priors to be translated to Stan code.
#' Objects of class `PriorHalfNormal` should not be created directly but by
#' the constructor [prior_half_normal()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for the half normal stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution. In a half normal
#' prior, constraint is `mu`
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family prior classes
.prior_half_normal <- setClass(
  "PriorHalfNormal",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "normal({{object@mu}}, {{object@sigma}})",
    constraint = "<lower={{object@mu}}>"
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior half-normal distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/normal-distribution.html>
#'
#' @return Object of class [`PriorHalfNormal`][PriorHalfNormal-class].
#' @export
#' @family priors
#' @examples
#' hcp <- prior_half_normal(1, 1)
prior_half_normal <- function(mu, sigma) {
  .prior_half_normal(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "PriorHalfNormal",
  definition = function(object) {
    cat("Half normal Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("mu", "sigma"),
        R = c("location", "scale"),
        Value = c(object@mu, object@sigma)
      ),
      row.names = FALSE, right = FALSE
    )
    print(h_glue("Constraints: {{eval_constraints(object)}}"))
  }
)

# plot ----
#' @rdname plot
#' @examples
#' plot(prior_half_normal(0, 1), xlim = c(-20, 20))
#' plot(prior_half_normal(0, 2), xlim = c(-20, 20), col = 2, add = TRUE)
setMethod(
  f = "plot",
  signature = c("PriorHalfNormal", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qnorm(c(0.5, 0.995), mean = x@mu, sd = x@sigma)
    density_fun <- function(values) {
      ifelse(
        values < x@mu,
        0,
        stats::dnorm(values, mean = x@mu, sd = x@sigma)
      )
    }
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
