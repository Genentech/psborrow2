
#' Sample from Stan model
#'
#' Method to sample from compiled Stan model and
#' return a `CmdStanMCMC` object with draws.
#'
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#'
#' @rdname mcmc_sample
#'
#' @export
mcmc_sample <- function(x, ...) {
  UseMethod("mcmc_sample", x)
}

#' Default method for sampling from Stan model
#'
#' Method to throw errors when `mcmc_sample()` is
#' called using classes for which it is not supported.
#'
#' @param x object to be passed on to the method
#' @param ... other arguments passed on to the method
#'
#' @rdname mcmc_sample
#'
#' @method mcmc_sample default
#'
#' @export
mcmc_sample.default <- function(x, ...) {
  if (length(class(x)) > 1) {
    stop("\nObjects of class `", paste0(class(x), collapse = "` / `"), "` not supported by `mcmc_sample()`.\n",
         "For analyses on a single dataset, pass an object of class `Analysis` created by `create_analysis_obj()`.\n",
         "For simulation analyses, pass an object of class `Simulation` created by `create_simulation_obj()`."
    )
  } else if (length(class(x)) == 1) {
    stop(paste0("\nObjects of class `", class(x), "` not supported by `mcmc_sample()`.\n",
         "For analyses on a single dataset, pass an object of class `Analysis` created by `create_analysis_obj()`.\n",
         "For simulation analyses, pass an object of class `Simulation` created by `create_simulation_obj()`."))
  }
}

#' Sample from Stan model contained in `Analysis` object
#'
#' @param x an object of class `Analysis` created by
#' create_analysis_obj()
#' @param iter_warmup The number of warmup iterations to run per chain.
#' The default is 1000.
#' @param iter_sampling The number of post-warmup iterations to run per chain.
#' The default is 10000.
#' @param chains The number of Markov chains to run. The default is 4.
#' @param ... additional arguments passed to the $sample() method of a
#' cmdstanr Stan model.
#' See https://mc-stan.org/cmdstanr/reference/model-method-sample.html
#'
#' @include create_analysis_obj.R
#'
#' @rdname mcmc_sample
#'
#' @method mcmc_sample `Analysis`
#' @return An object of class `CmdStanMCMC`
#' @export
#'
#' @examples
#'
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = normal_prior(0, 1000)
#'   ),
#'   outcome = weib_ph_surv_dist(
#'     "time",
#'     "cnsr",
#'     shape_prior = normal_prior(0, 1000)
#'   ),
#'   borrowing = borrowing_details(
#'     "BDB",
#'     "ext",
#'     exponential_prior(.001),
#'     baseline_prior = normal_prior(0, 1000)
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 1000))
#' )
#'
#' mcmc_results <- mcmc_sample(anls)
#'
mcmc_sample.Analysis <- function(x,
                                 iter_warmup = 1000L,
                                 iter_sampling = 10000L,
                                 chains = 4L,
                                 verbose = FALSE,
                                 ...) {
  if(verbose) {
    x@model_and_data$stan_model$sample(
      data = x@model_and_data$data_in,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      chains = chains,
      ...
    )
  } else {
    suppressMessages(
      x@model_and_data$stan_model$sample(
        data = x@model_and_data$data_in,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        chains = chains,
        refresh = 0L,
        ...
      )
    )
  }
}
