#' @include prior_class.R

# Internal constructor
.cauchy_prior <- setClass(
  "CauchyPrior",
  contains = "Prior",
  slots = c(
    mu = "numeric",
    sigma = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "cauchy({{object@mu}},{{object@sigma}})",
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
#' @param mu location
#' @param sigma scale (>0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/cauchy-distribution.html>
#'
#' @return object of class `CauchyPrior`
#' @export
#' @family priors
#' @examples
#' cp <- cauchy_prior(1, 1)
cauchy_prior <- function(mu, sigma) {
  .cauchy_prior(mu = mu, sigma = sigma)
}

# summary ----
setMethod(
  f = "summary",
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

    xlim <- stats::qcauchy(c(0.005, 0.995), location = object@mu, scale = object@sigma)
    x <- seq(from = xlim[1], to = xlim[2], length = 300)
    y <- stats::dcauchy(x, location = object@mu, scale = object@sigma)
    plot_pdf(x, y)
  }
)
