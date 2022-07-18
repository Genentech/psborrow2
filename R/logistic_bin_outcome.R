#' @include outcome_class.R

# Internal constructor
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
#' @return object of class `LogisticBinaryOutcome`
#' @export
#'
#' @examples
#' lg <- logistic_bin_outcome()
logistic_bin_outcome <- function() {
  .logistic_bin_outcome()
}
