#' `OutcomeBinaryLogistic` class
#'
#' A class for defining a logistic regression with a binary outcome
#' to be translated to Stan code.
#' Objects of class `OutcomeBinaryLogistic` should not be created directly but by
#' the constructor [outcome_bin_logistic()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `OutcomeBinaryLogistic`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `OutcomeBinaryLogistic`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `OutcomeBinaryLogistic`.
#' @slot binary_var character. Variable used for outcome in `OutcomeBinaryLogistic` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @include outcome_class.R helpers.R
#' @family outcome
.outcome_bin_logistic <- setClass(
  "OutcomeBinaryLogistic",
  contains = "BinaryOutcome",
  prototype = list(
    n_param = 0L,
    likelihood_stan_code =
      h_glue("
         for (i in 1:N) {
            target += bernoulli_logit_lupmf(y[i] | lp[i]) * weight[i];
         }")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Bernoulli distribution with logit parametrization
#'
#' @param binary_var character. Name of binary (1/0 or TRUE/FALSE) outcome variable in the
#' model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `Details` for more information.
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log odds. The interpretation of the `baseline_prior` differs
#' slightly between borrowing methods selected.
#' - \emph{Dynamic borrowing using `borrowing_hierarchical_commensurate()`}: the `baseline_prior` for Bayesian Dynamic Borrowing refers
#' to the log odds of the external control arm.
#' - \emph{Full borrowing} or \emph{No borrowing} using `borrowing_full()` or `borrowing_none()`: the `baseline_prior` for
#' these borrowing methods refers to the log odds for the
#' internal control arm.
#'
#' @return Object of class [`OutcomeBinaryLogistic`][OutcomeBinaryLogistic-class].
#' @export
#' @family outcome models
#'
#' @examples
#' lg <- outcome_bin_logistic(
#'   binary_var = "response",
#'   baseline_prior = prior_normal(0, 1000)
#' )
outcome_bin_logistic <- function(binary_var,
                                 baseline_prior,
                                 weight_var = "") {
  assert_string(binary_var)
  assert_string(weight_var)
  assert_class(baseline_prior, "Prior")
  has_weight <- isTRUE(weight_var != "")
  .outcome_bin_logistic(
    binary_var = binary_var,
    baseline_prior = baseline_prior,
    weight_var = weight_var,
    likelihood_stan_code = h_glue("
      for (i in 1:N) {
        target += bernoulli_logit_lupmf(y[i] | lp[i]){{weight}};
      }",
      weight = if (has_weight) " * weight[i]" else ""
    ),
    data_stan_code = h_glue("
      array[N] int y;
      {{weight}}",
      weight = if (has_weight) "vector[N] weight;" else ""
    )
  )
}


#' Legacy function for binary logistic regression
#'
#' Please use `outcome_bin_logistic()` instead.
#' @param ... Deprecated arguments to `logistic_bin_outcome`.
#' 
#' @return
#' This function does not return a value. When called, it triggers an error 
#' message indicating that `logistic_bin_outcome()` is deprecated and that 
#' `outcome_bin_logistic()` should be used instead.
#' 
#' @export
logistic_bin_outcome <- function(...) {
  .Defunct(
    "outcome_bin_logistic",
    "psborrow2",
    "`logistic_bin_outcome()` is deprecated. Use `outcome_bin_logistic()` instead."
  )
}
