#' @include prior.R

# Internal constructor
.exponential_prior <- setClass(
   "ExponentialPrior",
   contains = "Prior",
   slots = c(beta = "numeric"),
   prototype = list(
      n_param = 1L,
      stan_code = "exponential(beta={{object@beta}})"
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
#' @param beta inverse scale
#'
#' @return object of class "ExponentialPrior"
#' @export
#'
#' @examples
#' ep <- exponential_prior(1)
exponential_prior <- function(beta) {
   .exponential_prior(beta = beta)
}
