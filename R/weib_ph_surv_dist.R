#' @include surv_dist.R
#' @include prior.R
#' @include normal_prior.R

# Internal constructor
.weib_ph_surv_dist <- setClass(
   "WeibullPHSurvDist",
   contains = "SurvDist",
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
         ",.open="{{",.close="}}"),
      model_stan_code =
         glue::glue("
         for (i in 1:N) {
            if (cens[i] == 0) {
               target += weibull_ph_lccdf(time[i] | shape_weibull, exp_l[i] );
            } else {
               target += weibull_ph_lpdf(time[i] | shape_weibull, exp_l[i] );
            }
         }", .open="{{",.close="}}"),
      param_stan_code = "real<lower=0> shape_weibull; ",
      param_priors = list(
         shape_weibull = exponential_prior(beta = 0.0001)
      )
   ),
   validity = function(object) {
      if (!is(object@param_priors[['shape_weibull']], "Prior")) {
         return("shape_weibull must be of class 'Prior'")
      }
      return(TRUE)
   }
)

#' Weibull survival distribution (proportional hazards formulation)
#'
#' @param shape_prior (optional) Prior class object for the Weibull shape
#' parameter
#'
#' @return object of class "WeibullPHSurvDist"
#' @export
#'
#' @examples
#' ws <- weib_ph_surv_dist(exponential_prior(1))
weib_ph_surv_dist <-  function(shape_prior = NULL) {
   if (is.null(shape_prior)) {
      .weib_ph_surv_dist()
   } else {
      .weib_ph_surv_dist(
         param_priors = list(
            shape_weibull = shape_prior
         )
      )
   }
}
