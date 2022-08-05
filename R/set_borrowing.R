
#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set prior for commensurability parameter tau
#'
#' @param method One of:
#' - 'BDB' for Bayesian Dynamic Borrowing
#' - 'Full borrowing' for pooling of historical and concurrent controls
#' - 'No borrowing' for evaluating only the internal comparison, ignoring
#' historical controls
#' @param baseline_prior object of class `Prior`
#' specifying prior distribution for the baseline (ie the prior for the
#' external control) log hazard rate or the log odds.
#' @param ext_flag_col character specifying the name of the column in
#' the model matrix that corresponds to the external control flag (1/0 or T/F)
#' @param tau_prior object of class `Prior`. Only needed for method = 'BDB'
#'
#' @return an object of class `Borrowing`
#' @export
#'
#' @include borrowing_class.R
#'
#' @examples
#' sb <- set_borrowing("Full borrowing", normal_prior(0, 1000))
set_borrowing <- function(method,
                          baseline_prior = NULL,
                          ext_flag_col = NULL,
                          tau_prior = NULL) {
   # Additional checks not clear in class definition

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
