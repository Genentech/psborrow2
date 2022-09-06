#' Specify Borrowing Model
#'
#' Specify type of borrowing and, for Bayesian Dynamic Borrowing,
#' set hyperprior for commensurability parameter tau.
#'
#' @param method character. The type of borrowing to perform. It
#' must be one of: `'BDB'`, `'Full borrowing'`, or `'No borrowing'`. See _Details_ for
#' more information.
#' @param ext_flag_col character. The name of the column in
#' the data matrix that corresponds to the external control flag (`1`/`0` or
#' `TRUE`/`FALSE`). This identifies a patient as belonging to the external
#' control cohort.
#' @param tau_prior Object of class `Prior` defining the hyperprior on the
#' "commensurability parameter". See `Details` for more information.
#'
#' @details
#' ## Method

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
#' ## External Control
#'
#' The `ext_flag_col` argument refers to the column in the data matrix that
#' contains the flag indicating a patient is from the external control cohort.
#' While this column is not used in 'Full borrowing', it must still be
#' specified.
#'
#' ## Tau Prior
#'
#' The `tau_prior` argument specifies the hyperprior on the precision parameter
#' commonly referred to as the commensurability parameter.
#' See \href{https://doi.org/10.1002/pst.1589}{Viele et. al. 2014} for more
#' details.
#' This hyperprior determines (along with the comparability of the outcomes
#' between internal and external controls) how much borrowing of the external
#' control group will be performed.
#' Example hyperpriors include largely uninformative inverse gamma distributions
#' \[e.g., `gamma_prior(alpha = .001, beta = .001)`\] as well as more
#' informative distributions \[e.g., `gamma_prior(alpha = 1, beta = .001`)\],
#' though any distribution \eqn{x \in (0, \infty)} can be used. Distributions
#' with more density at higher values of \eqn{x} (i.e., higher precision)
#' will lead to more borrowing.
#'
#' @references Viele, K., Berry, S., Neuenschwander, B., Amzal, B., Chen, F.,
#'  Enas, N., Hobbs, B., Ibrahim, J.G., Kinnersley, N., Lindborg, S., Micallef,
#'   S., Roychoudhury, S. and Thompson, L. (2014),
#'   Use of historical control data for assessing treatment effects in clinical trials.
#'   __Pharmaceut. Statist., 13: 41--54__. \doi{10.1002/pst.1589}
#'
#' @return Object of class [`Borrowing`][Borrowing-class].
#' @export
#'
#' @include borrowing_class.R
#'
#' @examples
#' sb <- borrowing_details(
#'   method = "BDB",
#'   ext_flag_col = "ext",
#'   tau_prior = gamma_prior(0.001, 0.001)
#' )
#'
borrowing_details <- function(method,
                              ext_flag_col,
                              tau_prior = NULL) {
  # Additional checks and neater errors than in class definition
  assert_choice(method, c("Full borrowing", "No borrowing", "BDB"))
  assert_string(ext_flag_col)

  if (method == "BDB") {
    if (is.null(tau_prior)) stop("When method='BDB', tau_prior must be specified")
    assert_class(tau_prior, "Prior")
  }

  .borrowing_class(
    method = method,
    ext_flag_col = ext_flag_col,
    tau_prior = tau_prior
  )
}
