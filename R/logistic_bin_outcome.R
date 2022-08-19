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
            target += bernoulli_logit_lupmf(y[i] | lp[i]);
         }", .open = "{{", .close = "}}")
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Bernoulli distribution with logit parametrization
#'
#' @param binary_var Name of binary (1/0 or TRUE/FALSE) outcome variable in the
#' model matrix
#'
#' @return object of class `LogisticBinaryOutcome`
#' @export
#' @family outcome models
#'
#' @examples
#' lg <- logistic_bin_outcome(binary_var = "response")
logistic_bin_outcome <- function(binary_var) {
  assert_string(binary_var)
  .logistic_bin_outcome(binary_var = binary_var)
}
