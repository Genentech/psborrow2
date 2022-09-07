
#' @rdname mcmc_sample
setMethod(
  "mcmc_sample",
  signature = "ANY",
  definition = function(x, ...) {
    stop(
      "\nObjects of class `", paste0(class(x), collapse = "` / `"), "` not supported by `mcmc_sample()`.\n",
      "For analyses on a single dataset, pass an object of class `Analysis` created by `create_analysis_obj()`.\n",
      "For simulation analyses, pass an object of class `Simulation` created by `create_simulation_obj()`."
    )
  }
)

#' Sample from Stan model contained in `Analysis` object
#'
#' @param iter_warmup The number of warm up iterations to run per chain.
#' The default is 1000.
#' @param iter_sampling The number of post-warm up iterations to run per chain.
#' The default is 10000.
#' @param chains The number of Markov chains to run. The default is 4.
#' @param verbose logical. Whether to print sampler updates (`TRUE`) or not
#' (`FALSE`)
#'
#' @include create_analysis_obj.R
#'
#' @rdname mcmc_sample
#' @return An object of class `CmdStanMCMC`
#' @export
#'
#' @examples
#' ## Analysis objects
#' anls <- create_analysis_obj(
#'   data_matrix = example_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = normal_prior(0, 1000)
#'   ),
#'   outcome = weib_ph_surv_dist(
#'     "time",
#'     "cnsr",
#'     shape_prior = normal_prior(0, 1000),
#'     baseline_prior = normal_prior(0, 1000)
#'   ),
#'   borrowing = borrowing_details(
#'     "BDB",
#'     "ext",
#'     exponential_prior(.001)
#'   ),
#'   treatment = treatment_details("trt", normal_prior(0, 1000))
#' )
#'
#' mcmc_results <- mcmc_sample(anls)
#'
setMethod(
  "mcmc_sample",
  signature = "Analysis",
  definition = function(x,
                        iter_warmup = 1000L,
                        iter_sampling = 10000L,
                        chains = 4L,
                        verbose = FALSE,
                        ...) {
    if (!isTRUE(x@ready_to_sample)) stop("Cannot sample object. Create object using `create_analysis_obj()`")

    if (verbose) {
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
)
