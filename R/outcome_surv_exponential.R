#' `OutcomeSurvExponential` Class
#'
#' A class for defining a time-to-event survival analysis with an
#' exponential survival distribution.
#' Objects of class `OutcomeSurvExponential` should not be created directly
#' but by the constructor [outcome_surv_exponential()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvExponential`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvExponential`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `OutcomeSurvExponential`.
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
.outcome_surv_exponential <- setClass(
  "OutcomeSurvExponential",
  contains = "TimeToEvent",
  prototype = list(
    n_param = 0L,
    likelihood_stan_code =
      h_glue("
         for (i in 1:N) {
            if (cens[i] == 1) {
               target += exponential_lccdf(time[i] | elp[i] );
            } else {
               target += exponential_lpdf(time[i] | elp[i] );
            }
         }")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Exponential survival distribution
#'
#' @param time_var character. Name of time variable column in model matrix
#' @param cens_var character. Name of the censorship variable flag in model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `Details` for more information.
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log hazard rate. The interpretation of the `baseline_prior` differs
#' slightly between borrowing methods selected.
#' - \emph{Dynamic borrowing using `borrowing_hierarchical_commensurate()`}: the `baseline_prior` for Bayesian Dynamic Borrowing
#' refers to the log hazard rate of the external control arm.
#' - \emph{Full borrowing} or \emph{No borrowing} using `borrowing_full()` or `borrowing_none()`: the `baseline_prior` for
#' these borrowing methods refers to the log hazard rate for the
#' internal control arm.
#' @return Object of class [`OutcomeSurvExponential`][OutcomeSurvExponential-class].
#' @export
#' @family outcome models
#'
#' @examples
#' es <- outcome_surv_exponential(
#'   time_var = "time",
#'   cens_var = "cens",
#'   baseline_prior = prior_normal(0, 1000)
#' )
outcome_surv_exponential <- function(time_var, cens_var, baseline_prior, weight_var = "") {
  assert_string(time_var)
  assert_string(cens_var)
  assert_string(weight_var)
  assert_class(baseline_prior, "Prior")
  has_weight <- isTRUE(weight_var != "")
  .outcome_surv_exponential(
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
        weight = if (weight_var != "") " * weight[i]" else ""
      ),
    data_stan_code = h_glue("
      vector[N] time;
      vector[N] cens;
      {{weight}}",
      weight = if (has_weight) "vector[N] weight;" else ""
    )
  )
}

#' Legacy function for the exponential survival distribution
#'
#' Please use `outcome_surv_exponential()` instead.
#' @param ... Deprecated arguments to `exp_surv_dist()`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `exp_surv_dist()` is deprecated and that 
#' `outcome_surv_exponential()` should be used instead.
#' 
#' @export
exp_surv_dist <- function(...) {
  .Defunct(
    "outcome_surv_exponential",
    "psborrow2",
    "`exp_surv_dist()` is deprecated. Use `outcome_surv_exponential()` instead."
  )
}
