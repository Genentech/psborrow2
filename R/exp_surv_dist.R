#' @include outcome_class.R

# Internal constructor
.exp_surv_dist <- setClass(
   "ExponentialSurvDist",
   contains = "TimeToEvent",
   prototype = list(
      n_param = 0L,
      likelihood_stan_code =
         glue::glue("
         for (i in 1:N) {
            if (cens[i] == 1) {
               target += exponential_lccdf(time[i] | elp[i] );
            } else {
               target += exponential_lpdf(time[i] | elp[i] );
            }
         }", .open = "{{", .close = "}}"
         )
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
