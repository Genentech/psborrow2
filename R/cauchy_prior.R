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
      stan_code = "cauchy({{object@mu}},{{object@sigma}})"
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
#' @return object of class "CauchyPrior"
#' @export
#' @family priors
#' @examples
#' cp <- cauchy_prior(1, 1)
cauchy_prior <- function(mu, sigma) {
   .cauchy_prior(mu = mu, sigma = sigma)
}
