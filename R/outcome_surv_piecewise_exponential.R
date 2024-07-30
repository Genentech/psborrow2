#' `OutcomeSurvPiecewiseExponential` Class
#'
#' A class for defining a time-to-event survival analysis with a piecewise
#' exponential survival distribution.
#' Objects of class `OutcomeSurvPiecewiseExponential` should not be created directly
#' but by the constructor [outcome_surv_piecewise_exponential()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvExponential`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvExponential`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `OutcomeSurvPiecewiseExponential`.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @include outcome_class.R
#' @family outcome
.outcome_surv_piecewise_exponential <- setClass(
  "OutcomeSurvPiecewiseExponential",
  contains = "TimeToEvent",
  slots = c(
    start_times = "numeric"
  ),
  prototype = list(
    n_param = 0L,
    likelihood_stan_code = ""
  )
)

#' Piecewise Exponential survival distribution
#'
#' @param time_var character. Name of time variable column in model matrix
#' @param cens_var character. Name of the censorship variable flag in model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param baseline_prior `Prior`. Object of class `Prior` specifying prior distribution for the baseline outcome.
#' @param start_times numeric. A vector of times to define periods.
#'
#' @return Object of class [`OutcomeSurvPiecewiseExponential`][OutcomeSurvPiecewiseExponential-class].
#' @export
#' @family outcome models
#'
#' @examples
#' es <- outcome_surv_piecewise_exponential(
#'   time_var = "time",
#'   cens_var = "cens",
#'   baseline_prior = prior_normal(0, 1000),
#'   start_times = c(0, 10, 20)
#' )
outcome_surv_piecewise_exponential <- function(time_var, cens_var, start_times, baseline_prior, weight_var = "") {
  assert_string(time_var)
  assert_string(cens_var)
  assert_string(weight_var)
  assert_numeric(start_times, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1, sorted = TRUE)
  assert_class(baseline_prior, "Prior")
  has_weight <- isTRUE(weight_var != "")
  .outcome_surv_piecewise_exponential(
    time_var = time_var,
    cens_var = cens_var,
    baseline_prior = baseline_prior,
    weight_var = weight_var,
    likelihood_stan_code = 
      h_glue("
         for (i in 1:N) {
            if (cens[i] == 1) {
               target += exponential_lccdf(time[i] | elp[i] ){{weight}};
            } else {
               target += exponential_lpdf(time[i] | elp[i] ){{weight}};
            }
         }",
        weight = if (has_weight) " * weight[i]" else ""
      ),
    data_stan_code = h_glue(
      "
      vector[N] time;
      vector[N] cens;
      int<lower = 1> M;
      {{weight}}",
      weight = if (has_weight) "vector[N] weight;" else ""
    )
  )
}