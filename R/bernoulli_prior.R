#' @include prior_class.R

# Internal constructor
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

#' Prior binomial distribution
#'
#' @param theta probability (in \[0, 1\])
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/bernoulli-distribution.html>
#'
#' @return object of class `BernoulliPrior`
#' @export
#' @family priors
#' @examples
#' bp <- bernoulli_prior(0.23)
bernoulli_prior <- function(theta) {
  .bernoulli_prior(theta = theta)
}

# summary ----
setMethod(
  f = "summary",
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

    x <- c(0, 1)
    y <- c(1 - object@theta, object@theta)
    plot_pmf(x, y)
  }
)

# plot ----
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
