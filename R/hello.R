#' Personal greeting
#'
#' @description Greet a person and appropriately capitalize their name.
#'
#' @param name Your name (character string; e.g. "john").
#'
#' @return A character string.
#' @export
#'
#' @examples
#' hello("james bond")
hello <- function(name = "your name") {
  name <- paste(toupper(substring(name, 1, 1)), substring(name, 2),
    sep = "", collapse = " "
  )
  print(paste("Hello,", name))
}
