
#' Plot Prior Objects
#'
#' Plot prior distributions as densities. Continuous distributions are plotted as curves and
#' discrete distributions as bar plots.
#'
#' @name plot
#' @aliases plot
#'
#' @param x Object inheriting from `Prior`
#' @param y Not used.
#' @param add logical. Add density to existing plot.
#' @param ... Optional arguments for plotting.
#'
#' @export
#' @details
#' Plot ranges are selected by default to show 99% of the density for unbounded distributions.
#' The limits can be changed by specifying `xlim = c(lower, upper)`.
#'
#' Colors, line types, and other typical [par()] parameters can be used.
#'
if (!isGeneric("plot")) setGeneric("plot", function(x, y, ...) standardGeneric("plot"))


#' Get Stan Code
#'
#' Gets the completed Stan code from an object with parameters inserted
#'
#' @name get_stan_code
#' @aliases get_stan_code
#'
#' @param object Object
#' @param ... Optional arguments passed to methods.
#'
#' @returns A `character` with Stan code.
#' @export
#'
setGeneric("get_stan_code", function(object, ...) standardGeneric("get_stan_code"))

#' Get Variables
#'
#' Gets the data variable names from an object.
#'
#' @name get_vars
#' @aliases get_vars
#'
#' @param object Object
#' @returns A `character` vector containing variable names
#' @export
#'
setGeneric("get_vars", function(object) standardGeneric("get_vars"))
