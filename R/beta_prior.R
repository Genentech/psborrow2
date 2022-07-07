#' @include prior.R

# Internal constructor
.beta_prior <- setClass(
   "BetaPrior",
   contains = "Prior",
   slots = c(alpha = "numeric",
             beta = "numeric"),
   prototype = list(
      n_param = 2L,
      stan_code = "beta(alpha={{object@alpha}}, beta={{object@beta}})"
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
#' @param alpha alpha
#' @param beta beta
#'
#' @return object of class "BetaPrior"
#' @export
#'
#' @examples
#' bp <- beta_prior(9,235)
beta_prior <- function(alpha, beta) {
   .beta_prior(alpha = alpha, beta = beta)
}
