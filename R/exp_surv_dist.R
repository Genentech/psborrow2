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
         }", .open = "{{", .close = "}}")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Exponential survival distribution
#'
#' @param time_var Name of time variable column in model matrix
#' @param cens_var Name of the censorship variable flag in model matrix
#'
#' @return object of class "ExponentialSurvDist"
#' @export
#' @family Outcome models
#'
#' @examples
#' es <- exp_surv_dist(time_var = 'time', cens_var = 'cens')
exp_surv_dist <- function(time_var, cens_var) {
  assert_string(time_var)
  assert_string(cens_var)
  .exp_surv_dist(time_var = time_var, cens_var = cens_var)
}
