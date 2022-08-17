
#' Plot Prior Objects
#'
#' Plot prior distributions as densities. Continuous distributions are plotted as curves and
#' discrete distributions as bar plots.
#'
#' @name plot
#' @aliases plot
#'
#' @param x Object inheriting from `Prior`
#' @param add logical. Add density to existing plot.
#' @param ... Optional arguments for plotting.
#'
#' @details
#' Plot ranges are selected by default to show 99% of the density for unbounded distributions.
#' The limits can be changed by specifying `xlim = c(lower, upper)`.
#'
#' Colors, line types, and other typical [par()] parameters can be used.
#'
#' @examples
#' plot(normal_prior(0, 1))
#' plot(cauchy_prior(0, 2), col = 2, add = TRUE)
#' plot(poisson_prior(5), xlim = c(0, 15))
NULL
