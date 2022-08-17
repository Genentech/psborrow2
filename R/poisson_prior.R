#' @include prior_class.R

# Internal constructor
.poisson_prior <- setClass(
  "PoissonPrior",
  contains = "Prior",
  slots = c(lambda = "numeric"),
  prototype = list(
    n_param = 1L,
    stan_code = "poisson({{object@lambda}})",
    constraint = "<lower=0>"
  ),
  validity = function(object) {
    if (object@lambda <= 0) {
      return("lambda must be >0")
    }
    return(TRUE)
  }
)

#' Prior poisson distribution
#'
#' @param lambda rate (>0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/poisson.html>
#'
#' @return object of class `PoissonPrior`
#' @export
#' @family priors
#' @examples
#' pp <- poisson_prior(100)
poisson_prior <- function(lambda) {
  .poisson_prior(lambda = lambda)
}

# summary ----
setMethod(
  f = "summary",
  signature = "PoissonPrior",
  definition = function(object) {
    cat("Poisson Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("lambda"),
        R = c("lambda"),
        Value = c(object@lambda)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))
  }
)

# plot ----

#' @rdname plot
#' @examples
#' plot(poisson_prior(5), xlim = c(0, 15))
setMethod(
  f = "plot",
  signature = c("PoissonPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qpois(c(0.005, 0.995), lambda = x@lambda)
    density_fun <- function(values) stats::dpois(values, lambda = x@lambda)
    dist_type <- "discrete"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
