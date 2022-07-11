#' @include surv_dist.R

# Internal constructor
.exp_surv_dist <- setClass(
   "ExponentialSurvDist",
   contains = "SurvDist",
   prototype = list(
      n_param = 0L,
      model_stan_code =
        "
         for (i in 1:N) {
         if (cens[i] == 0) {
            target += exponential_lccdf(time[i], exp_lambda[i] );
        }
         else {
            target += exponential_lpdf(time[i] | exp_lambda[i] );
        }
      }"
   ),
   validity = function(object) {
      return(TRUE)
   }
)

#' Exponential survival distribution
#'
#' @return object of class "ExponentialSurvDist"
#' @export
#'
#' @examples
#' es <- exp_surv_dist()
exp_surv_dist <-  function() {
   .exp_surv_dist()
}
