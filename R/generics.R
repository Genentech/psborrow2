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

#' Sample from Stan model
#'
#' Method to sample from compiled Stan model and
#' return a `CmdStanMCMC` object with draws.
#'
#' @param x object to sample, such as `Analysis` (created with [create_analysis_obj()]) or `Simulation`.
#' @param ... additional arguments passed to the $sample() method of a
#' `cmdstanr` Stan model.
#' See https://mc-stan.org/cmdstanr/reference/model-method-sample.html
#'
#' @rdname mcmc_sample
#'
#' @export
setGeneric("mcmc_sample", function(x, ...) standardGeneric("mcmc_sample"))

#' Show guide for objects with guides
#'
#' Show the guide in `Simulation` objects.
#'
#' @param object `Simulation` object
#'
#' @rdname show_guide
#'
#' @export
#'
setGeneric("show_guide", function(object) standardGeneric("show_guide"))

#' Get results for `MCMCSimulationResults` objects
#'
#' Get the results `data.frame` from `MCMCSimulationResults` objects.
#'
#' @param object `MCMCSimulationResults` object
#'
#' @rdname get_results
#'
#' @export
#'
setGeneric("get_results", function(object) standardGeneric("get_results"))

#' Get `CmdStanModel` objects for `MCMCSimulationResults`
#'
#' Show the `CmdStanModel` objects from `MCMCSimulationResults` objects.
#'
#' @param object `MCMCSimulationResults` object
#'
#' @rdname get_cmd_stan_models
#'
#' @export
#'
setGeneric("get_cmd_stan_models", function(object) standardGeneric("get_cmd_stan_models"))

#' Generate Data from Object
#'
#' @param x object
#' @param ... Other arguments passed to methods
#'
#' @rdname generate
#'
#' @export
setGeneric("generate", function(x, ...) standardGeneric("generate"))

#' Trim Rows from Data Matrix Based on Borrowing object type 
#' 
#' @param borrowing_object borrowing object
#' @param analysis_object analysis object
#' 
#' @rdname trim_rows
#' 
#' @export
setGeneric("trim_rows", function(borrowing_object, analysis_object) standardGeneric("trim_rows"))

#' Trim columns from Data Matrix Based on Borrowing object type 
#' 
#' @param borrowing_object borrowing object
#' @param analysis_object analysis object
#' 
#' @rdname trim_cols
#' 
#' @export
setGeneric("trim_cols", function(borrowing_object, analysis_object) standardGeneric("trim_cols"))