#' Plot Probability Density Function Values
#'
#' @param x values
#' @param y probability density values `y = f(x)`
#' @param ... passed to [plot()]
#'
#' Plots the density values as a curve with the lower vertical limit set to 0.
#'
#' @export
#'
#' @examples
#' x <- seq(-2, 2, len = 100)
#' y <- dnorm(x)
#' plot_pdf(x, y)
plot_pdf <- function(x, y, ...) {
  plot(x,
    y,
    type = "l",
    ylim = c(0, max(y[is.finite(y)])),
    ylab = "density",
    ...
  )
}

#' Plot Probability Mass Function Values
#'
#' @param x values
#' @param y probability mass values `y = f(x)`
#' @param ... passed to [barplot()]
#'
#' Plots the probability values as a barplot.
#'
#' @export
#'
#' @examples
#' x <- seq(0, 5)
#' y <- dpois(x, lambda = 2)
#' plot_pmf(x, y)
plot_pmf <- function(x, y, ...) {
  graphics::barplot(
    height = y,
    width = 0.2,
    names.arg = x,
    ylim = c(0, 1),
    ...
  )
}
