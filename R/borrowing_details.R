#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set prior for commensurability parameter tau
#'
#' @param method This argument specifies the type of borrowing to perform. It
#' must be one of:
#' - \strong{'BDB'} for Bayesian Dynamic Borrowing. In Bayesian Dynamic
#' Borrowing, external control information is borrowed to the extent that the
#' outcomes (i.e., log hazard rates or log odds) are similar between
#' external and internal control populations. See Viele et. al. 2014
#' (https://doi.org/10.1002/pst.1589).
#' - \strong{'Full borrowing'} for pooling of historical and concurrent controls.
#' There is no distinction between patients in the internal and external
#' control arms.
#' - \strong{'No borrowing'} for evaluating only the internal comparison,
#' ignoring historical controls. Note that this method will filter the
#' model matrix (if you have included any external control patients).
#' @param baseline_prior object of class `Prior`
#' specifying prior distribution for the baseline log hazard rate or
#' log odds, depending on the outcome type. The interpretation of this parameter
#' depends on the `method` argument:
#' - \strong{BDB}: the `baseline_prior` for Bayesian Dynamic Borrowing refers
#' to the log hazard rate or log odds of the external control arm.
#' - \strong{Full borrowing} or \strong{No borrowing}: the `baseline_prior` for
#' these borrowing methods refers to the log hazard rate or log odds for the
#' internal control arm.
#' @param ext_flag_col character specifying the name of the column in
#' the model matrix that corresponds to the external control flag (1/0 or T/F).
#' This argument is required for 'BDB' and is ignored by
#' 'Full borrowing'. If the argument is specified with method 'No borrowing',
#' then the method will filter the cohort to include only patients in the
#' internal control. If no argument is specified for method 'No borrowing', it
#' will be assumed that the model matrix contains only internal control
#' patients.
#'
#' @param tau_prior object of class `Prior`. This is the prior on the
#' "commensurability parameter", and determines, in addition to the
#' comparability of the outcomes between internal and external controls, how
#' much borrowing will occur. Example priors include largely uninformative
#' priors (e.g., `gamma_prior(alpha = .001, beta = .001)`) and somewhat informative
#' priors (e.g., `gamma_prior(alpha = 1, beta = 001`).
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
                              baseline_prior = NULL,
                              ext_flag_col = NULL,
                              tau_prior = NULL) {
  # Additional checks and neater errors than in class definition

  if (!method %in% c(
    "Full borrowing",
    "No borrowing",
    "BDB"
  )) {
    stop("method must be one of: 'BDB', 'Full borrowing', 'No borrowing'")
  }

  if (method == "BDB" && (
    is.null(tau_prior) ||
      is.null(ext_flag_col) ||
      is.null(baseline_prior)
  )) {
    stop(paste0(
      "When method = 'BDB', ext_flag_col, ",
      "tau_prior, and baseline_prior must be ",
      "specified"
    ))
  }

  if (is.null(baseline_prior)) {
    stop(paste0(
      "baseline_prior must be ",
      "specified"
    ))
  }

  if (method == "Full borrowing" && !is.null(ext_flag_col)) {
    message("Ignoring ext_flag_col for full borrowing")
  }

  if (method == "No borrowing" && !is.null(ext_flag_col)) {
    message("Filtering model matrix to exclude external control patients")
  }

  if (method == "No borrowing" && is.null(ext_flag_col)) {
    warning(paste0(
      "Not excluding any patients because 'ext_flag_col' is not ",
      "specified. If you have external control patients you ",
      "would like to exclude, specify the external control flag ",
      "column in argument 'ext_flag_col'"
    ))
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
