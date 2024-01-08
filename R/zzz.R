.onAttach <- function(libname, pkgname) {
  if (!is_cmdstanr_available()) {
    packageStartupMessage(
      "psborrow2 uses cmdstanr for compiling and sampling from models, but it does not seem to be installed.\n",
      "To install:\n",
      "install.packages(\"cmdstanr\", repos = c(\"https://mc-stan.org/r-packages/\", getOption(\"repos\")))"
    )
  } else if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    possible_paths <- unique(c(
      cmdstanr::cmdstan_default_install_path(),
      Sys.getenv("CMDSTAN"),
      Sys.getenv("CMDSTAN_PATH"),
      "/root/.cmdstan",
      "~/.cmdstan"
    ))
    possible_paths <- possible_paths[dir.exists(possible_paths)]

    if (length(possible_paths)) {
      for (try_path in possible_paths) {
        new_path <- tryCatch(
          suppressMessages(cmdstanr::set_cmdstan_path(try_path)),
          warning = function(w) NULL,
          error = function(e) NULL
        )
      }
      if (!is.null(new_path)) {
        packageStartupMessage("CmdStan path set to: ", new_path)
      }
    } else {
      packageStartupMessage("psborrow2 could not identify CmdStan path. Please use cmdstanr::set_cmdstan_path()")
    }
  }
  return(invisible(NULL))
}
