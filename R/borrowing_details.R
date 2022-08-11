#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set hyperprior for commensurability parameter tau
#'
#' @param method This argument specifies the type of borrowing to perform. It
#' must be one of: 'BDB', 'Full borrowing', or 'No borrowing'. See `details` for
#' more information.
#' @param baseline_prior object of class `Prior`
#' specifying prior distribution for the baseline outcome.
#' See `details` for more information.
#' @param ext_flag_col character specifying the name of the column in
#' the model matrix that corresponds to the external control flag (`1/0` or
#' `T/F`).
#'
#' @param tau_prior object of class `Prior`. This is the hyperprior on the
#' "commensurability parameter". See `details` for more information.
#'
#' @details
#'
#' \strong{method}
#'
#' The `method` argument specifies the type of borrowing that will be
#' implemented. There are currently three types of borrowing that are supported:
#'
#' - \emph{'BDB'} for Bayesian Dynamic Borrowing. In Bayesian Dynamic
#' Borrowing, external control information is borrowed to the extent that the
#' outcomes (i.e., log hazard rates or log odds) are similar between
#' external and internal control populations. See
#' \href{https://doi.org/10.1002/pst.1589}{Viele et. al. 2014}.
#' - \emph{'Full borrowing'} for pooling of historical and concurrent controls.
#' There is no distinction between patients in the internal and external
#' control arms. While the `ext_flag_col` must still be specified, it is not
#' used.
#' - \emph{'No borrowing'} for evaluating only the internal comparison,
#' ignoring historical controls. Note that this method will filter the
#' model matrix based on values in `ext_flag_col`.
#'
#' Though the ultimate model specification is the same for 'Full borrowing'
#' and 'No borrowing', both are available as options to facilitate comparison
#' between methods.
#'
#' \strong{baseline_prior}
#'
#' The `baseline_prior` argument specifies the prior distribution for the
#' baseline log hazard rate or log odds, depending on the outcome type. The
#' interpretation of the `baseline_prior` differs slightly between methods:
#' - \emph{'BDB'}: the `baseline_prior` for Bayesian Dynamic Borrowing refers
#' to the log hazard rate or log odds of the external control arm.
#' - \emph{'Full borrowing'} or \emph{'No borrowing'}: the `baseline_prior` for
#' these borrowing methods refers to the log hazard rate or log odds for the
#' internal control arm.
#'
#' \strong{ext_flag_col}
#'
#' The `ext_flag_col` argument refers to the column in the model matrix that
#' contains the flag indicating a patient is from the external control cohort.
#' While this column is not used in 'Full borrowing', it must still be
#' specified.
#'
#' \strong{tau_prior}
#'
#' The `tau_prior` argument specifies the hyperprior on the precision parameter
#' commonly referred to as the commensurability parameter.
#' See \href{https://doi.org/10.1002/pst.1589}{Viele et. al. 2014} for more
#' details.
#' This hyperprior determines, in addition to the comparability of the outcomes
#' between internal and external controls, how much borrowing of the external
#' control group will be performed.
#' Example hyperpriors include largely uninformative inverse gamma distributions
#' \[e.g., `gamma_prior(alpha = .001, beta = .001)`\] as well as more
#' informative distributions \[e.g., `gamma_prior(alpha = 1, beta = .001`)\],
#' though any distribution \eqn{x\in (0, \infty)} can be used. Distributions
#' with more density at higher values of \eqn{x} (i.e., higher precision)
#' will lead to more borrowing.
#'
#' @return an object of class `Borrowing`
#' @export
#'
#' @include borrowing_class.R
#'
#' @examples
#' sb <- borrowing_details(
#'   "BDB",
#'   normal_prior(0, 1000),
#'   "ext",
#'   gamma_prior(0.001, 0.001)
#' )
#'
borrowing_details <- function(method,
                              baseline_prior,
                              ext_flag_col,
                              tau_prior = NULL) {
  # Additional checks and neater errors than in class definition

  assert_choice(method, c("Full borrowing", "No borrowing", "BDB"))

  if (method == "BDB" && is.null(tau_prior)) {
    stop(paste0(
      "When method = 'BDB', tau prior must ",
      "be specified"
    ))
  }

  if (method == "Full borrowing") {
    message("Will ignore ext_flag_col for full borrowing")
  }

  if (method == "No borrowing") {
    message("Will filter model matrix to exclude external control patients")
  }

  if (method == "BDB" && !is(tau_prior, "Prior")) {
    stop("tau prior bust be of class `Prior`")
  }

  if (method == "BDB" && !is(baseline_prior, "Prior")) {
    stop("baseline_prior prior bust be of class `Prior`")
  }

  .borrowing_class(
    method = method,
    ext_flag_col = ext_flag_col,
    tau_prior = tau_prior,
    baseline_prior = baseline_prior
  )
}
