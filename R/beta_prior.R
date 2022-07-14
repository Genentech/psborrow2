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
      stan_code = "beta({{object@alpha}}, {{object@beta}})"
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
#' @return object of class "BetaPrior"
#' @export
#' @family priors
#' @examples
#' bp <- beta_prior(9, 235)
beta_prior <- function(alpha, beta) {
   .beta_prior(alpha = alpha, beta = beta)
}
