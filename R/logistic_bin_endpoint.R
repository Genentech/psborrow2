#' @include outcome_class.R

# Internal constructor
.logistic_bin_endpoint <- setClass(
   "LogisticBinaryEndpoint",
   contains = "BinaryEndpoint",
   prototype = list(
      n_param = 0L,
      likelihood_stan_code =
         glue::glue("
         for (i in 1:N) {
            target += bernoulli_logit_lupmf(y[i] | lp[i]);
         }", .open = "{{", .close = "}}"
         )
   ),
   validity = function(object) {
      return(TRUE)
   }
)

#' Bernoulli distribution with logit parametrization
#'
#' @return object of class "LogisticBinaryEndpoint"
#' @export
#'
#' @examples
#' lg <- logistic_bin_endpoint()
logistic_bin_endpoint <-  function() {
   .logistic_bin_endpoint()
}
