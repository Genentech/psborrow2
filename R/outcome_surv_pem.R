#' `OutcomeSurvPEM` Class
#'
#' A class for defining a time-to-event survival analysis with a
#' piecewise survival distribution.
#' Objects of class `OutcomeSurvPEM` should not be created directly
#' but by the constructor [outcome_surv_pem()].
#'
#' @slot function_stan_code character. stan function code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvPEM`.
#' @slot param_stan_code character. stan parameter code block containing text to interpolate into stan model.
#' Empty string for `OutcomeSurvPEM`.
#' @slot likelihood_stan_code character. stan model likelihood code block containing text
#' to interpolate into stan model.
#' @slot n_param integer. Number of ancillary parameters for the model to estimate (0).
#' @slot param_priors list. Named list of prior distributions on the ancillary parameters in the model.
#' Empty for `OutcomeSurvPEM`.
#' @slot time_var character. Variable used for time in `TimeToEvent` objects.
#' @slot cens_var character. Variable used for censoring in `TimeToEvent` objects.
#' @slot baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' @slot name_beta_trt. Named vector for beta_trt.
#' @slot name_exp_trt. Named vector for exponentiated beta_trt
#' @slot alpha_type. How to interpret alpha.
#' @slot name_addnl_params. Named vector for additional parameters.
#' @slot n_periods. Number of periods.
#' @include outcome_class.R
#' @family outcome
.outcome_surv_pem <- setClass(
  "OutcomeSurvPEM",
  contains = "TimeToEvent",
  slots = list(
    n_periods = "integer",
    cut_points = "numeric"
  ),
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
         }"),
    n_periods = 1L
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Piecewise exponential survival distribution
#'
#' @param time_var character. Name of time variable column in model matrix
#' @param cens_var character. Name of the censorship variable flag in model matrix
#' @param weight_var character. Optional name of variable in model matrix for weighting the log likelihood.
#' @param baseline_prior `Prior`. Object of class `Prior`
#' specifying prior distribution for each cut point.
#' See `Details` for more information.
#' @param cut_points numeric. Vector of internal cut points for the piecewise exponential model. Note: the choice of
#' cut points will impact the amount of borrowing between arms when dynamic borrowing methods are selected. It is
#' recommended to choose cut points that contain an equal number of events within each interval. Please include only internal
#' cut points in the vector. For instance, for cut points of [0, 15], (15, 20], (20, Inf], the vector should be c(15, 20).
#' If you pass cut-points beyond the follow-up of the data, you will receive an informative warning when calling
#' `create_analysis_object()` and these cut points will be ignored.
#'
#' @details
#' ## Baseline Prior
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log hazard rate within each cutpoint. Currently, there is no option to
#' consider different baseline priors within each cut point.
#' The interpretation of the `baseline_prior` differs
#' slightly between borrowing methods selected.
#' - \emph{Dynamic borrowing using `borrowing_hierarchical_commensurate()`}: the `baseline_prior` for Bayesian Dynamic Borrowing
#' refers to the log hazard rate of the external control arm.
#' - \emph{Full borrowing} or \emph{No borrowing} using `borrowing_full()` or `borrowing_none()`: the `baseline_prior` for
#' these borrowing methods refers to the log hazard rate for the internal control arm.
#' @return Object of class [`OutcomeSurvPEM`][OutcomeSurvPEM-class].
#' @export
#' @family outcome models
#'
#' @examples
#' es <- outcome_surv_pem(
#'   time_var = "time",
#'   cens_var = "cens",
#'   baseline_prior = prior_normal(0, 1000),
#'   cut_points = c(10, 15, 30)
#' )
outcome_surv_pem <- function(time_var, cens_var, baseline_prior, weight_var = "", cut_points) {

  # Standard input checks
  assert_string(time_var)
  assert_string(cens_var)
  assert_string(weight_var)
  assert_class(baseline_prior, "Prior")
  assert_numeric(cut_points)

  # Cut points
  cut_points_is_sorted <- all(diff(cut_points) > 0)
  if (!cut_points_is_sorted) {
    stop("`cut_points` must be sorted in ascending order.")
  }

  cut_points_neg0 <- any(cut_points <= 0)
  cut_points_inf <- any(cut_points == Inf)
  if (cut_points_neg0 | cut_points_inf) {
    stop("`cut_points` must be positive, non-infinite and exclude 0. Just put internal cutpoints, the model will automatically add 0 and Inf.")
  }

  n_cuts <- length(cut_points)
  if (n_cuts < 1) {
    stop("`cut_points` must have at least one element.")
  }

  # Create the object
  has_weight <- isTRUE(weight_var != "")
  .outcome_surv_pem(
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
    ),
    cut_points = cut_points
  )
}

# show ----
setMethod(
  f = "show",
  signature = "Outcome",
  definition = function(object) {
    cat("Outcome object with class", class(object)[1], "\n\n")
    cat("Outcome variables:\n")
    print(get_vars(object))
    cat("\n")
    cat("Baseline prior:\n")
    show(object@baseline_prior)

    if (!is.null(object@param_priors)) {
      cat("\n")
      for (i in names(object@param_priors)) {
        cat(i, "prior:\n")
        show(object@param_priors[[i]])
      }
    }

    cat("Cut points:", paste0(object@cut_points, collapse = ", "))
  }
)
