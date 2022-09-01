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
    xlim <- range(x) + c(-0.5, 0.5)
    ylim <- c(0, max(y))
    plot(x, y, type = "n", xaxt = "n", xlab = "", ylab = "", ..., xlim = xlim, ylim = ylim)
    graphics::axis(side = 1, at = x, col.ticks = NA)
  }
  graphics::rect(x - 0.5, 0, x + 0.5, y, col = rep(col, length(y)), ...)
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


#' Get constraints from a list of `Prior`s
#'
#' @param cov_obj A `Covariates` object.
#'
#' @return A `matrix` with columns "lower" and "upper" with rows for each `Prior`.
#' @examples
#' psborrow2:::get_covariate_constraints(
#'   add_covariates(
#'     c("cov1", "cov2", "cov3"),
#'     list(
#'       normal_prior(0, 10),
#'       beta_prior(0.3, 0.3),
#'       gamma_prior(30, 1)
#'     )
#'   )
#' )
#'
get_covariate_constraints <- function(cov_obj) {
  n_covs <- length(cov_obj@covariates)
  if (is(cov_obj@priors, "Prior")) {
    cons <- rep(parse_constraint(cov_obj@priors@constraint), each = n_covs)
    cons <- matrix(cons, ncol = 2, dimnames = list(NULL, c("lower", "upper")))
  } else {
    cons <- t(vapply(cov_obj@priors, function(p) parse_constraint(p@constraint), numeric(2L)))
  }
  assert_numeric(cons, any.missing = FALSE, len = 2 * n_covs)
  cons
}

#' Extract Upper and Lower Bounds from Constraint String
#'
#' @param s Character. Constraint string, of the form `"<lower = 0, upper = 1>"`.
#'
#' @return
#' A list with upper and lower bounds. Any unspecified bounds are set to `-Inf` or `Inf`.
#' @examples
#' psborrow2:::parse_constraint("<lower=0>")
#' psborrow2:::parse_constraint("<lower=0, upper=1>")
#' psborrow2:::parse_constraint("")
parse_constraint <- function(s) {
  assert_character(s)
  s <- gsub("[<>[:space:]]", "", s)
  s_list <- strsplit(s, ",")[[1]]

  lower <- as.numeric(
    gsub("lower[[:space:]]*=[[:space:]]*", "", s_list[grepl("lower", s_list)])
  )

  upper <- as.numeric(
    gsub("upper[[:space:]]*=[[:space:]]*", "", s_list[grepl("upper", s_list)])
  )

  c(lower = max(lower, -Inf, na.rm = TRUE), upper = min(upper, Inf, na.rm = TRUE))
}
