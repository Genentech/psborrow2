#' @include surv_dist.R

# Internal constructor
.weib_ph_surv_dist <- setClass(
   "WeibullPHSurvDist",
   contains = "SurvDist",
   prototype = list(
      n_param = 1L,
      function_stan_code =
         "

         ",
      model_stan_code =
         "
         for (i in 1:N) {
            if (cens[i] == 0) {
               target += weibull_ph_lccdf(time[i] | shape_weibull, exp_l[i] );
            }
            else {
               target += weibull_ph_lpdf(time[i] | shape_weibull, exp_l[i] );
            }
         }",

   ),
   validity = function(object) {
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
#' ws <- weib_ph_surv_dist(normal_prior(0, 100))
weib_ph_surv_dist <-  function() {
   weib_ph_surv_dist()
}
