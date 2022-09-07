#' `ExponentialSurvDist` Class
#'
#' A class for defining a time-to-event survival analysis with an
#' exponential survival distribution.
#' Objects of class `ExponentialSurvDist` should not be created directly
#' but by the constructor [exp_surv_dist()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `ExponentialSurvDist`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `ExponentialSurvDist`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `ExponentialSurvDist`.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @include outcome_class.R
#' @family outcome
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
#' @param time_var character. Name of time variable column in model matrix
#' @param cens_var character. Name of the censorship variable flag in model matrix
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `Details` for more information.
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log hazard rate. The interpretation of the `baseline_prior` differs
#' slightly between methods selected in `borrowing_details()`:
#' - \emph{'BDB'}: the `baseline_prior` for Bayesian Dynamic Borrowing refers
#' to the log hazard rate of the external control arm.
#' - \emph{'Full borrowing'} or \emph{'No borrowing'}: the `baseline_prior` for
#' these borrowing methods refers to the log hazard rate for the
#' internal control arm.
#' @return Object of class [`ExponentialSurvDist`][ExponentialSurvDist-class].
#' @export
#' @family outcome models
#'
#' @examples
#' es <- exp_surv_dist(
#'   time_var = "time",
#'   cens_var = "cens",
#'   baseline_prior = normal_prior(0, 1000)
#' )
exp_surv_dist <- function(time_var, cens_var, baseline_prior) {
  assert_string(time_var)
  assert_string(cens_var)
  assert_class(baseline_prior, "Prior")
  .exp_surv_dist(
    time_var = time_var,
    cens_var = cens_var,
    baseline_prior = baseline_prior
  )
}
