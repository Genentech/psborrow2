#' `OutcomeContinuousNormal` class
#'
#' A class for defining a regression with a normal outcome
#' to be translated to Stan code.
#' Objects of class `OutcomeContinuousNormal` should not be created directly but by
#' the constructor [outcome_cont_normal()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `OutcomeContinuousNormal`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `OutcomeContinuousNormal`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `OutcomeContinuousNormal`.
#' @slot continuous_var character. Variable used for outcome in `OutcomeContinuousNormal` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @include outcome_class.R helpers.R prior_half_cauchy.R
#' @family outcome
.outcome_cont_normal <- setClass(
  "OutcomeContinuousNormal",
  contains = "ContinuousOutcome",
  prototype = list(
    n_param = 0L,
    likelihood_stan_code =
      h_glue("
         for (i in 1:N) {
            target += normal_lupdf(y[i] | lp[i], std_dev_outcome) * weight[i];
         }"),
    param_stan_code = "real<lower=0> std_dev_outcome; ",
    param_priors = list(
      std_dev_outcome = prior_half_cauchy(1, 5)
    )
  ),
  validity = function(object) {
    msg <- NULL
    if (!test_class(object@param_priors[["std_dev_outcome"]], "Prior")) {
      msg <- c(msg, "`prior_params` slot must contain `std_dev_outcome` object with class `Prior`")
    }
    if (is.null(msg)) TRUE else msg
  }
)

#' Normal Outcome Distribution
#'
#' @param continuous_var character. Name of continuous outcome variable in the
#' model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `Details` for more information.
#' @param std_dev_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the standard deviation of the outcome distribution (i.e. "sigma").
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' intercept of the linear model. The interpretation of the `baseline_prior` differs
#' slightly between borrowing methods selected.
#' - \emph{Dynamic borrowing using `borrowing_hierarchical_commensurate()`}: the `baseline_prior` for Bayesian Dynamic Borrowing
#' refers to the intercept of the external control arm.
#' - \emph{Full borrowing} or \emph{No borrowing} using `borrowing_full()` or `borrowing_none()`: the `baseline_prior` for
#' these borrowing methods refers to the intercept for the
#' internal control arm.
#'
#' @return Object of class [`OutcomeContinuousNormal`][OutcomeContinuousNormal-class].
#' @export
#' @family outcome models
#'
#' @examples
#' norm <- outcome_cont_normal(
#'   continuous_var = "tumor_size",
#'   baseline_prior = prior_normal(0, 100),
#'   std_dev_prior = prior_half_cauchy(1, 5)
#' )
outcome_cont_normal <- function(continuous_var,
                                baseline_prior,
                                std_dev_prior,
                                weight_var = "") {
  assert_string(continuous_var)
  assert_string(weight_var)
  assert_class(baseline_prior, "Prior")
  assert_class(std_dev_prior, "Prior")
  has_weight <- isTRUE(weight_var != "")
  .outcome_cont_normal(
    continuous_var = continuous_var,
    baseline_prior = baseline_prior,
    weight_var = weight_var,
    param_priors = list(
      std_dev_outcome = std_dev_prior
    ),
    likelihood_stan_code = h_glue("
      for (i in 1:N) {
        target += normal_lupdf(y[i] | lp[i], std_dev_outcome){{weight}};
      }",
      weight = if (has_weight) " * weight[i]" else ""
    ),
    data_stan_code = h_glue("
      array[N] real y;
      {{weight}}",
      weight = if (has_weight) "vector[N] weight;" else ""
    )
  )
}
