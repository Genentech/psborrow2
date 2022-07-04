#' @include prior.R

# Internal constructor
.normal_prior <- setClass(
   "NormalPrior",
   contains = "Prior",
   slots = c(mu = "numeric",
             sigma = "numeric"),
   prototype = list(
      n_param = 2L,
      stan_code = "normal(mu={{object@mu}}, sigma={{object@sigma}})"
   ),
   validity = function(object) {
      if(object@sigma <= 0) {
         return("sigma must be >0")
      }
      return(TRUE)
   }
)

#' Prior normal distribution
#'
#' @param mu location
#' @param sigma scale (>0)
#'
#' @return object of class "NormalPrior"
#' @export
#'
#' @examples
#' np <- normal_prior(1,1)
normal_prior <- function(mu, sigma) {
   .normal_prior(mu = mu, sigma = sigma)
}
