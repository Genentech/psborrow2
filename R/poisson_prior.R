#' @include prior_class.R

# Internal constructor
.poisson_prior <- setClass(
   "PoissonPrior",
   contains = "Prior",
   slots = c(lambda = "numeric"),
   prototype = list(
      n_param = 1L,
      stan_code = "poisson(lambda={{object@lambda}})"
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
#' @param lambda lambda
#'
#' @return object of class "PoissonPrior"
#' @export
#'
#' @examples
#' pp <- poisson_prior(100)
poisson_prior <- function(lambda) {
   .poisson_prior(lambda = lambda)
}
