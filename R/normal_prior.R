#' @include prior_class.R

# Internal constructor
.normal_prior <- setClass(
  "NormalPrior",
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
#' @param mu location
#' @param sigma scale (>0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/normal-distribution.html>
#'
#' @return object of class `NormalPrior`
#' @export
#' @family priors
#' @examples
#' np <- normal_prior(1, 1)
normal_prior <- function(mu, sigma) {
  .normal_prior(mu = mu, sigma = sigma)
}

# show ----
setMethod(
  f = "show",
  signature = "NormalPrior",
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
#' plot(normal_prior(1, 2))
setMethod(
  f = "plot",
  signature = c("NormalPrior", "missing"),
  definition = function(x, y, add = FALSE, ...) {
    limits <- stats::qnorm(c(0.005, 0.995), mean = x@mu, sd = x@sigma)
    density_fun <- function(values) stats::dnorm(values, mean = x@mu, sd = x@sigma)
    dist_type <- "continuous"
    callNextMethod(default_limits = limits, density_fun = density_fun, dist_type = dist_type, add = add, ...)
  }
)
