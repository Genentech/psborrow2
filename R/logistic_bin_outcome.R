#' `LogisticBinaryOutcome` class
#'
#' A class for defining a logistic regression with a binary outcome
#' to be translated to Stan code.
#' Objects of class `LogisticBinaryOutcome` should not be created directly but by
#' the constructor [logistic_bin_outcome()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `LogisticBinaryOutcome`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `LogisticBinaryOutcome`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `LogisticBinaryOutcome`.
#' @slot binary_var character. Variable used for outcome in `LogisticBinaryOutcome` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @include outcome_class.R
#' @family outcome
.logistic_bin_outcome <- setClass(
  "LogisticBinaryOutcome",
  contains = "BinaryOutcome",
  prototype = list(
    n_param = 0L,
    likelihood_stan_code =
      glue::glue("
         for (i in 1:N) {
            target += bernoulli_logit_lupmf(y[i] | lp[i]) * weight[i];
         }", .open = "{{", .close = "}}")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Bernoulli distribution with logit parametrization
#'
#' @param binary_var character. Name of binary (1/0 or TRUE/FALSE) outcome variable in the
#' model matrix
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `Details` for more information.
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log odds. The interpretation of the `baseline_prior` differs
#' slightly between methods selected in `borrowing_details()`:
#' - \emph{'BDB'}: the `baseline_prior` for Bayesian Dynamic Borrowing refers
#' to the log odds of the external control arm.
#' - \emph{'Full borrowing'} or \emph{'No borrowing'}: the `baseline_prior` for
#' these borrowing methods refers to the log odds for the
#' internal control arm.
#'
#' @return Object of class [`LogisticBinaryOutcome`][LogisticBinaryOutcome-class].
#' @export
#' @family outcome models
#'
#' @examples
#' lg <- logistic_bin_outcome(
#'   binary_var = "response",
#'   baseline_prior = normal_prior(0, 1000)
#' )
logistic_bin_outcome <- function(binary_var,
                                 baseline_prior,
                                 weight_var = "") {
  assert_string(binary_var)
  assert_string(weight_var)
  assert_class(baseline_prior, "Prior")
  .logistic_bin_outcome(
    binary_var = binary_var,
    baseline_prior = baseline_prior,
    weight_var = weight_var,
    likelihood_stan_code =
      h_glue(
        "
         for (i in 1:N) {
            target += bernoulli_logit_lupmf(y[i] | lp[i]){{weight}};
         }",
        weight = if (weight_var != "") " * weight[i]" else ""
      )
  )
}
