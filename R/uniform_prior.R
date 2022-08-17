#' @include prior_class.R

# Internal constructor
.uniform_prior <- setClass(
  "UniformPrior",
  contains = "Prior",
  slots = c(
    alpha = "numeric",
    beta = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "uniform({{object@alpha}}, {{object@beta}})",
    constraint = "<lower={{object@alpha}}, upper = {{object@beta}}>"
  ),
  validity = function(object) {
    if (object@beta <= object@alpha) {
      return("beta must be > alpha")
    }
    return(TRUE)
  }
)

#' Prior uniform distribution
#'
#' @param alpha lower bound
#' @param beta upper bound (>`alpha`)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/uniform-distribution.html>
#'
#' @return object of class `UniformPrior`
#' @export
#' @family priors
#' @examples
#' up <- uniform_prior(0, 1)
uniform_prior <- function(alpha, beta) {
  constraint <- glue::glue("<lower={{alpha}}, upper={{beta}}>", .open = "{{", .close = "}}")
  .uniform_prior(alpha = alpha, beta = beta, constraint = constraint)
}


# show ----
setMethod(
  f = "show",
  signature = "UniformPrior",
  definition = function(object) {
    cat("Uniform Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("alpha", "beta"),
        R = c("min", "max"),
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
#' plot(uniform_prior(1, 2), xlim = c(0, 3))
setMethod(
  f = "plot",
  signature = c("UniformPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- c(x@alpha, x@beta)
    density_fun <- function(values) stats::dunif(values, min = x@alpha, max = x@beta)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
