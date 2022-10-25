#' `WeibullPHSurvDist` Class
#'
#' A class for defining a time-to-event survival analysis with a
#' Weibull proportional hazards survival distribution.
#' Objects of class `WeibullPHSurvDist` should not be created directly
#' but by the constructor [weib_ph_surv_dist()].
#'
#' @slot function_stan_code character. Stan function code block containing text to interpolate into Stan model.
#' @slot param_stan_code character. Stan parameter code block containing text to interpolate into Stan model.
#' @slot likelihood_stan_code character. Stan model likelihood code block containing text
#' to interpolate into Stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (1).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @include outcome_class.R
#' @include prior_class.R
#' @include normal_prior.R
#' @include exponential_prior.R
#' @family outcome
.weib_ph_surv_dist <- setClass(
  "WeibullPHSurvDist",
  contains = "TimeToEvent",
  prototype = list(
    n_param = 1L,
    function_stan_code =
      h_glue("
          real weibull_ph_lpdf(real y, real alpha, real lambda) {
              real lprob = log(alpha) + log(lambda) + (alpha - 1) * log(y) - lambda * (y^alpha);
              return lprob;
          }

          real weibull_ph_lcdf(real y, real alpha, real lambda) {
              real lprob = log(1-exp(-lambda * y^alpha));
              return lprob;
          }

          real weibull_ph_lccdf(real y, real alpha, real lambda) {
              real lprob = -lambda * y^alpha;
              return lprob;
          }
         "),
    likelihood_stan_code =
      h_glue("
         for (i in 1:N) {
            if (cens[i] == 1) {
               target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i] );
            } else {
               target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i] );
            }
         }"),
    param_stan_code = "real<lower=0> shape_weibull; ",
    param_priors = list(
      shape_weibull = exponential_prior(beta = 0.0001)
    )
  ),
  validity = function(object) {
    check_class(object@param_priors[["shape_weibull"]], "Prior")
    return(TRUE)
  }
)

#' Weibull survival distribution (proportional hazards formulation)
#'
#' @param time_var character. Name of time variable column in model matrix
#' @param cens_var character. Name of the censorship variable flag in model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param shape_prior `Prior` class object for the Weibull shape
#' parameter. Default is `exponential_prior(beta = 0.0001)`.
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
#'
#' @return Object of class [`WeibullPHSurvDist`][WeibullPHSurvDist-class].
#' @export
#' @family outcome models
#'
#' @examples
#' ws <- weib_ph_surv_dist(
#'   time_var = "time",
#'   cens_var = "cens",
#'   shape_prior = exponential_prior(1),
#'   baseline_prior = normal_prior(0, 1000)
#' )
weib_ph_surv_dist <- function(time_var,
                              cens_var,
                              shape_prior,
                              baseline_prior,
                              weight_var = "") {
  assert_string(time_var)
  assert_string(cens_var)
  assert_string(weight_var)
  assert_class(shape_prior, "Prior")
  assert_class(baseline_prior, "Prior")
  .weib_ph_surv_dist(
    time_var = time_var,
    cens_var = cens_var,
    weight_var = weight_var,
    param_priors = list(
      shape_weibull = shape_prior
    ),
    baseline_prior = baseline_prior,
    likelihood_stan_code = h_glue(
      "
       for (i in 1:N) {
          if (cens[i] == 1) {
             target += weibull_ph_lccdf(time[i] | shape_weibull, elp[i] ){{weight}};
          } else {
             target += weibull_ph_lpdf(time[i] | shape_weibull, elp[i] ){{weight}};
          }
       }",
      weight = if (weight_var != "") " * weight[i]" else ""
    )
  )
}
