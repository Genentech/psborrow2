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
#' @param ... passed to [plot()] and [rect()]
#' @param col Fill color of bars.
#' @param add Add bars to existing plot.
#' @param xlim Limits of x axis.
#'
#' Plots the probability values as a barplot.
#'
#' @export
#'
#' @examples
#' x <- seq(0, 5)
#' y <- dpois(x, lambda = 2)
#' plot_pmf(x, y)
plot_pmf <- function(x, y, ..., col = "grey", add = FALSE, xlim) {
  if (isFALSE(add)) {
    xlim <- extendrange(if (missing(xlim)) range(x) else xlim, f = 0.1)
    ylim <- c(0, max(y))
    plot(x, y, type = "n", xaxt = "n", xlab = "", ylab = "", ..., xlim = xlim, ylim = ylim)
    axis(side = 1, at = x, col.ticks = NA)
  }
  rect(x - 0.5, 0, x + 0.5, y, col = rep(col, length(y)), ...)
}

#' Glue Strings with Default Arguments
#'
#' Calls [glue::glue()] with `.open = '{{'` and `.close = '}}'`
#' to simplify gluing Stan code.
#'
#' @param ... Arguments passed to [glue::glue()]
#'
#' @return A string.
#' @noRd
#' @examples
#' name <- "Tom"
#' psborrow2:::h_glue("hello, my name is {{name}}.")
h_glue <- function(...) {
  glue::glue(..., .open = "{{", .close = "}}", .envir = parent.frame())
}
