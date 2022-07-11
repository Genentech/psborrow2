
#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set prior for commensurability parameter tau
#'
#' @param method One of:
#' - 'BDB' for Bayesian Dynamic Borrowing
#' - 'Full borrowing' for pooling of historical and concurrent controls
#' - 'No borrowing' for evaluating only the internal comparison, ignoring
#' historical controls
#' @param tau_prior Object of class `Prior`. Only needed for method = 'BDB'
#'
#' @return an object of class `Borrowing`
#' @export
#'
#' @include borrowing_class.R
#'
#' @examples
#' sb <- set_borrowing('Full borrowing')
set_borrowing <- function(method,
                          tau_prior = NULL) {

   # Additional checks not clear in class definition
   if(method == "BDB" && !is(tau_prior, "Prior")) {
      stop("tau prior bust be of class `Prior`")
   }

   .borrowing_class(method = method,
                    tau_prior = tau_prior)
}
