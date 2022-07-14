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
      stan_code = "uniform({{object@alpha}}, {{object@beta}})"
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
#' @param alpha lower bounds
#' @param beta upper bounds
#'
#' @return object of class "UniformPrior"
#' @export
#' @family priors
#' @examples
#' up <- uniform_prior(0, 1)
uniform_prior <- function(alpha, beta) {
   .uniform_prior(alpha = alpha, beta = beta)
}
