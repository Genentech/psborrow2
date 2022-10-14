.onAttach <- function(libname, pkgname) {
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    possible_paths <- unique(c(
      cmdstan_default_install_path(),
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
        if (!is.null(new_path)) {
          packageStartupMessage("CmdStan path set to: ", new_path)
          return(invisible(NULL))
        }
      }
    }
  }
}
