
#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set prior for commensurability parameter tau
#'
#' @param method One of:
#' - 'BDB' for Bayesian Dynamic Borrowing
#' - 'Full borrowing' for pooling of historical and concurrent controls
#' - 'No borrowing' for evaluating only the internal comparison, ignoring
#' historical controls
#' @param ext_flag_col character specifying the name of the column in
#' the model matrix that corresponds to the external control flag (1/0 or T/F)
#' @param tau_prior object of class `Prior`. Only needed for method = 'BDB'
#' @param ext_log_hazard_rate_or_odds_prior object of class `Prior` specifying
#'  the prior distribution for the log hazard rate or the log odds
#' for the concurrent control arm.
#'
#' @return an object of class `Borrowing`
#' @export
#'
#' @include borrowing_class.R
#'
#' @examples
#' sb <- set_borrowing('Full borrowing')
set_borrowing <- function(method,
                          ext_flag_col = NULL,
                          tau_prior = NULL,
                          ext_log_hazard_rate_or_odds_prior = NULL) {

   # Additional checks not clear in class definition

   if(method == "BDB" && (
      is.null(tau_prior) |
      is.null(ext_flag_col) |
      is.null(ext_log_hazard_rate_or_odds_prior)
   )) {
      stop(paste0("When method = 'BDB', ext_flag_col, ",
           "tau_prior, and ext_log_hazard_rate_or_odds_prior must be ",
           "specified"))
   }

   if(method == "BDB" && !is(tau_prior, "Prior")) {
      stop("tau prior bust be of class `Prior`")
   }

   if(method == "BDB" && !is(ext_log_hazard_rate_or_odds_prior, "Prior")) {
      stop("ext_log_hazard_rate_or_odds_prior prior bust be of class `Prior`")
   }

   .borrowing_class(method = method,
                    ext_flag_col  = ext_flag_col,
                    tau_prior = tau_prior,
                    ext_log_hazard_rate_or_odds_prior =
                       ext_log_hazard_rate_or_odds_prior)
}
