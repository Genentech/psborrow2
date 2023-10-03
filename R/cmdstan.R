#' Check Stan
#'
#' @description Check whether `cmdstanr` is available and prints version and logistic example.
#' @param check_sampling Compile and sample from the "logistic" example model.
#'
#' @export
#'
#' @examples
#' check_cmdstanr()
check_cmdstanr <- function(check_sampling = FALSE) {
  cat("cmdstan version: \n")
  print(cmdstan_version())

  cat("Example program:\n")
  print_example_program(example = c("logistic"))

  if (check_sampling) {
    cat("Example program results:\n")
    cmdstanr_example("logistic", quiet = FALSE, chains = 1)
  }
}
