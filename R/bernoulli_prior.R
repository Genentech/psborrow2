#' @include prior.R

# Internal constructor
.bernoulli_prior <- setClass(
   "BernoulliPrior",
   contains = "Prior",
   slots = c(theta = "numeric"),
   prototype = list(
      n_param = 1L,
      stan_code = "bernoulli(theta={{object@theta}})"
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
#' @param theta theta
#'
#' @return object of class "BernoulliPrior"
#' @export
#'
#' @examples
#' bp <- bernoulli_prior(0.23)
bernoulli_prior <- function(theta) {
   .bernoulli_prior(theta = theta)
}
