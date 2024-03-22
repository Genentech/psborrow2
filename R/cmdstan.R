is_cmdstanr_available <- function() {
  requireNamespace("cmdstanr", quietly = TRUE)
}

#' Check Stan
#'
#' @description Check whether `cmdstanr` is available and prints version and logistic example.
#' @param check_sampling Compile and sample from the "logistic" example model.
#'
#' @return `check_cmdstanr()` prints results from checks.
#' @export
#' @importFrom utils packageVersion
#' @examples
#' check_cmdstanr()
check_cmdstanr <- function(check_sampling = FALSE) {
  if (!is_cmdstanr_available()) {
    cat("cmdstanr is not installed or is installed but not configured. \n")
  } else {
    cat("cmdstanr is installed.\n")
    cat("cmdstanr version: \n")
    cat(format(packageVersion("cmdstanr")), "\n")

    cat("cmdstan version: \n")
    cat(cmdstanr::cmdstan_version(error_on_NA = FALSE), "\n")

    cat("Example program:\n")
    cmdstanr::print_example_program(example = c("logistic"))

    if (check_sampling) {
      cat("Example program results:\n")
      cmdstanr::cmdstanr_example("logistic", quiet = FALSE, chains = 1)
    }
  }
}



#' @return `check_cmdstan()` returns `TRUE` if `CmdStan` seems to be installed, otherwise `FALSE`
#' @describeIn check_cmdstanr Check if the `CmdStan` command line tools are available.
#' @export
check_cmdstan <- function() {
  if (!is_cmdstanr_available()) {
    return(FALSE)
  }
  if (is.null(cmdstanr::cmdstan_version(FALSE))) {
    return(FALSE)
  }
  if (!file.exists(file.path(cmdstanr::cmdstan_path(), "make"))) {
    return(FALSE)
  }

  TRUE
}
