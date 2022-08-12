#' Check Stan
#'
#' @description Check whether `cmdstanr` is available.
#'
#' @return Prints version and logistic example.
#' @export
#'
#' @examples
#' check_cmdstanr()
check_cmdstanr <- function() {
  cat("cmdstan version: \n")
  print(cmdstan_version())

  cat("Example program:\n")
  print_example_program(example = c("logistic"))

  cat("Example program results:\n")
  cmdstanr_example("logistic", quiet = FALSE, chains = 1)
}
