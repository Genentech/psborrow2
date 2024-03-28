#' `PriorHalfCauchy` Class
#'
#' A class for defining half cauchy priors to be translated to Stan code.
#' Objects of class `PriorHalfCauchy` should not be created directly but by
#' the constructor [prior_half_cauchy()].
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for the half cauchy stan function parameters surrounded with
#' `{{` and `}}` to be replaced with [glue::glue()].
#' @slot n_param integer. Number of prior parameters (2).
#' @slot constraint character. Support of prior distribution. In a half cauchy
#' prior, constraint is `mu`
#' @slot mu numeric. Location.
#' @slot sigma numeric. Scale (>0).
#' @include prior_class.R
#' @family prior classes
.prior_half_cauchy <- setClass(
  "PriorHalfCauchy",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "cauchy({{object@mu}}, {{object@sigma}})",
    constraint = "<lower={{object@mu}}>"
  ),
  validity = function(object) {
    if (object@sigma <= 0) {
      return("sigma must be >0")
    }
    return(TRUE)
  }
)

#' Prior half-cauchy distribution
#'
#' @param mu numeric. Location.
#' @param sigma numeric. Scale (>0).
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/cauchy-distribution.html>
#'
#' @return Object of class [`PriorHalfCauchy`][PriorHalfCauchy-class].
#' @export
#' @family priors
#' @examples
#' hcp <- prior_half_cauchy(1, 1)
prior_half_cauchy <- function(mu, sigma) {
  .prior_half_cauchy(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "PriorHalfCauchy",
  definition = function(object) {
    cat("Half Cauchy Distribution\n")
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
#' plot(prior_half_cauchy(0, 1), xlim = c(-20, 20))
#' plot(prior_half_cauchy(0, 2), xlim = c(-20, 20), col = 2, add = TRUE)
setMethod(
  f = "plot",
  signature = c("PriorHalfCauchy", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qcauchy(c(0.5, 0.995), location = x@mu, scale = x@sigma)
    density_fun <- function(values) {
      ifelse(
        values < x@mu,
        0,
        stats::dcauchy(values, location = x@mu, scale = x@sigma)
      )
    }
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)


#' Legacy function for the half-cauchy prior
#'
#' Please use `prior_half_cauchy()` instead.
#' @param ... Deprecated arguments to `half_cauchy_prior()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `half_cauchy_prior()` is deprecated and that 
#' `prior_half_cauchy()` should be used instead. 
#'
#' @export
half_cauchy_prior <- function(...) {
  .Defunct(
    "prior_half_cauchy",
    "psborrow2",
    "`half_cauchy_prior()` is deprecated. Use `prior_half_cauchy()` instead."
  )
}
