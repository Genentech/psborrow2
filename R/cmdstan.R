#' Check Stan
#'
#' @description Check whether `cmdstanr` is available and prints version and logistic example.
#' @param check_sampling Compile and sample from the "logistic" example model.
#'
#' @return `check_cmdstanr()` prints results from checks.
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



#' @return `check_cmdstan()` returns `TRUE` if `CmdStan` seems to be installed, otherwise `FALSE`
#' @describeIn check_cmdstanr Check if the `CmdStan` command line tools are available.
check_cmdstan <- function() {
  if (isFALSE(cmdstanr::cmdstan_version(FALSE))) {
    return(FALSE)
  }
  if (!file.exists(file.path(cmdstanr::cmdstan_path(), "make"))) {
    return(FALSE)
  }
  TRUE
}
