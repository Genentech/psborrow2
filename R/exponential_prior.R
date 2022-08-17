#' @include prior_class.R

# Internal constructor
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
#' @param beta inverse scale (>0)
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

# summary ----
setMethod(
  f = "summary",
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
