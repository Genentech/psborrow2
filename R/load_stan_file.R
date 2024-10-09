#' Load a Stan `psborrow2` template
#'
#' This function loads a Stan template file from the package's 'inst/stan' directory.
#' @param ... subidrectories
#' @return template string
load_stan_file <- function(...) {
  # Construct the path within the package
  template_path <- system.file("stan", ..., package = "psborrow2")
  
  # Check if the file exists
  if (template_path == "") {
    stop("Template file not found at path: ", file.path("inst/stan", ...))
  }
  
  # Read the content of the file
  template_content <- paste(readLines(template_path), collapse = "\n")
  
  return(template_content)
}
