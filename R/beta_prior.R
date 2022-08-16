#' @include prior_class.R

# Internal constructor
.beta_prior <- setClass(
  "BetaPrior",
  contains = "Prior",
  slots = c(
    alpha = "numeric",
    beta = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "beta({{object@alpha}}, {{object@beta}})",
    constraint = "<lower=0, upper=1>"
  ),
  validity = function(object) {
    if (object@alpha < 0 || object@beta < 0) {
      return("Both alpha and beta must be >= 0")
    }
    return(TRUE)
  }
)

#' Prior beta distribution
#'
#' @param alpha shape (>=0)
#' @param beta shape (>=0)
#'
#' @details
#' Stan reference <https://mc-stan.org/docs/functions-reference/beta-distribution.html>
#'
#' @return object of class "BetaPrior"
#' @export
#' @family priors
#' @examples
#' bp <- beta_prior(9, 235)
beta_prior <- function(alpha, beta) {
  .beta_prior(alpha = alpha, beta = beta)
}


# summary ----
setMethod(
  f = "summary",
  signature = "BetaPrior",
  definition = function(object) {
    cat("Beta Distribution\n")
    cat("Parameters:\n")
    print.data.frame(
      data.frame(
        Stan = c("alpha", "beta"),
        R = c("shape1", "shape2"),
        Value = c(object@alpha, object@beta)
      ),
      row.names = FALSE, right = FALSE
    )
    if (object@constraint != "") print(h_glue("Constraints: {{object@constraint}}"))

    x <- seq(from = 0, to = 1, length = 300)
    y <- stats::dbeta(x, shape1 = object@alpha, shape2 = object@beta)
    plot_pdf(x, y)
  }
)
