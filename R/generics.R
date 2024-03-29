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
#' @return No return value, this function generates a plot in the current graphics device.
#' @export
#' @details
#' Plot ranges are selected by default to show 99% of the density for unbounded distributions.
#' The limits can be changed by specifying `xlim = c(lower, upper)`.
#'
#' Colors, line types, and other typical [par()] parameters can be used.
if (!isGeneric("plot")) setGeneric("plot", function(x, y, ...) standardGeneric("plot"))


#' Get Variables
#'
#' Gets the data variable names from an object.
#'
#' @name get_vars
#' @aliases get_vars
#'
#' @param object Object
#' @return A `character` vector containing variable names
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
#' @return A `data.frame` showing all simulation scenarios.
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
#' @return data.frame with simulation results.
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
#' @return List of lists of `CmdStanModel` objects for each model.
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
#' @return Object of class [`SimDataList`][SimDataList-class].
#' 
#' @export
setGeneric("generate", function(x, ...) standardGeneric("generate"))

#' Trim Rows from Data Matrix Based on Borrowing object type
#'
#' @param borrowing_object borrowing object
#' @param analysis_object analysis object
#'
#' @rdname trim_rows
setGeneric("trim_rows", function(borrowing_object, analysis_object) standardGeneric("trim_rows"))

#' Trim columns from Data Matrix Based on Borrowing object type
#'
#' @param borrowing_object borrowing object
#' @param analysis_object analysis object
#'
#' @rdname trim_cols
setGeneric("trim_cols", function(borrowing_object, analysis_object) standardGeneric("trim_cols"))

#' Create alpha string
#'
#' @param borrowing_object borrowing object
#' @param outcome_object outcome object
#'
#' @rdname create_alpha_string
setGeneric("create_alpha_string", function(borrowing_object, outcome_object) standardGeneric("create_alpha_string"))

#' Create tau string
#'
#' @param borrowing_object borrowing object
#'
#' @rdname create_tau_string
setGeneric("create_tau_string", function(borrowing_object) standardGeneric("create_tau_string"))

#' Create Stan Code for Model
#'
#' @param borrowing borrowing object
#' @param outcome outcome object
#' @param analysis_obj analysis object
#'
#' @rdname make_model_string_model
#' @return `glue` `character` containing the Stan code for the data block.
#' @export
#' @examples
#' anls_obj <- create_analysis_obj(
#'     data_matrix = example_matrix,
#'     outcome = outcome_surv_exponential(
#'       "time",
#'       "cnsr",
#'       baseline_prior = prior_normal(0, 1000)
#'     ),
#'     borrowing = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_exponential(.001)
#'     ),
#'     treatment = treatment_details(
#'       "trt",
#'       prior_normal(0, 1000)
#'     ),
#'     covariates = add_covariates(
#'       covariates = c("cov1", "cov2"),
#'       priors = prior_normal(0, 1000)
#'     )
#'   )
#' make_model_string_model(anls_obj@borrowing, anls_obj@outcome, anls_obj)
setGeneric("make_model_string_model", function(borrowing, outcome, analysis_obj) {
  standardGeneric("make_model_string_model")
})

#' @title Coerce a `psborrow2` object to a data frame
#'
#' @description Creates `data.frame` objects from various classes in `psborrow2`
#'
#' @name as_data_frame
#'
#' @param x object of type: [BaselineDataList-class]
#' @param ... Optional arguments for passed to [data.frame]
#' @return A `data.frame`
NULL

#' @title Combine objects in `psborrow2`
#'
#' @description Creates `data.frame` objects from various classes in `psborrow2`
#' @name c
#' @param x object of type: [SimDataList-class]
#' @param ... additional objects to combine
#' @return A combined object
NULL

#' Get Simulated Data from `SimDataList` object
#'
#' Retrieves the simulated data from a `SimDataList` object by index.
#'
#' @param object `SimDataList` object
#' @param index the index of the scenario (see guide with print(`SimDataList`))
#' @param dataset the dataset out of `n_datasets_per_param`
#' @return Simulated data as a data frame if the index is specified, else as a list
#' @export
setGeneric("get_data", function(object, index = 1, dataset = 1) standardGeneric("get_data"))

#' Set transformations in `BaselineObject` objects
#'
#' @param object `BaselineObject` object
#' @param ... Additional arguments passed to methods
#' @param overwrite logical. Overwrite existing transformations?
#' @return `BaselineObject` object with transformations
#' @export
setGeneric("set_transformations", function(object, ..., overwrite = FALSE) standardGeneric("set_transformations"))

#' Get method for Stan model
#' @param object `Analysis` object
#' @return String containing the Stan model
#' @export
setGeneric("get_stan_code", function(object) standardGeneric("get_stan_code"))
