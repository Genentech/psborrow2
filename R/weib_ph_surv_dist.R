#' @include outcome_class.R
#' @include prior_class.R
#' @include normal_prior.R

# Internal constructor
.weib_ph_surv_dist <- setClass(
  "WeibullPHSurvDist",
  contains = "TimeToEvent",
  prototype = list(
    n_param = 1L,
    function_stan_code =
      glue::glue("
          real weibull_ph_lpdf(real y, real alpha, real lambda) {
              real lprob = log(alpha) + log(lambda) + (alpha - 1) * log(y) - lambda * (y^alpha);
              return lprob;
          }

          real weibull_ph_lcdf(real y, real alpha, real lambda) {
              real lprob = log(1-exp(-lambda * y^alpha));
              return lprob;
          }

          real weibull_ph_lccdf(real y, real alpha, real lambda) {
              real lprob = -lambda * y^alpha;
              return lprob;
          }
         ", .open = "{{", .close = "}}"),
    likelihood_stan_code =
      glue::glue("
         for (i in 1:N) {
            if (cens[i] == 1) {
               target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i] );
            } else {
               target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i] );
            }
         }", .open = "{{", .close = "}}"),
    param_stan_code = "real<lower=0> shape_weibull; ",
    param_priors = list(
      shape_weibull = exponential_prior(beta = 0.0001)
    )
  ),
  validity = function(object) {
    check_class(object@param_priors[["shape_weibull"]], "Prior")
    return(TRUE)
  }
)

#' Weibull survival distribution (proportional hazards formulation)
#'
#' @param time_var Name of time variable column in model matrix
#' @param cens_var Name of the censorsing variable flag in model matrix
#'
#' @param shape_prior (optional) `Prior` class object for the Weibull shape
#' parameter
#'
#' @return object of class `WeibullPHSurvDist`
#' @export
#' @family Outcome models
#'
#' @examples
#' ws <- weib_ph_surv_dist(time_var = "time", cens_var = "cens", shape_prior = exponential_prior(1))
weib_ph_surv_dist <- function(time_var, cens_var, shape_prior = exponential_prior(beta = 0.0001)) {
  assert_string(time_var)
  assert_string(cens_var)
  .weib_ph_surv_dist(
    time_var = time_var,
    cens_var = cens_var,
    param_priors = list(
      shape_weibull = shape_prior
    )
  )
}
