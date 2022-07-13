#' @include prior_class.R

# Internal constructor
.gamma_prior <- setClass(
  "GammaPrior",
  contains = "Prior",
  slots = c(
    alpha = "numeric",
    beta = "numeric"
  ),
  prototype = list(
    n_param = 2L,
    stan_code = "gamma({{object@alpha}}, {{object@beta}})"
  ),
  validity = function(object) {
    if (object@alpha <= 0 && object@beta <= 0) {
      return("Both alpha and beta must be >= 0")
    }
    return(TRUE)
  }
)

#' Prior gamma distribution
#'
#' @param alpha shape
#' @param beta inverse scale
#'
#' @return object of class "GammaPrior"
#' @export
#'
#' @examples
#' gp <- gamma_prior(0.001, 0.001)
gamma_prior <- function(alpha, beta) {
  .gamma_prior(alpha = alpha, beta = beta)
}
