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
#' @param iter_warmup integer. The number of warm up iterations to run per chain.
#' The default is 1000.
#' @param iter_sampling integer. The number of post-warm up iterations to run per chain.
#' The default is 10000.
#' @param chains integer. The number of Markov chains to run. The default is 4.
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
#'     "BDB_HCP",
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
    # Input checks
    assert_class(x, "Analysis")
    assert_numeric(iter_warmup)
    assert_numeric(iter_sampling)
    assert_numeric(chains)
    assert_flag(verbose)

    if (!isTRUE(x@ready_to_sample)) stop("Cannot sample object. Create object using `create_analysis_obj()`")

    if (verbose) {
      x@model$sample(
        data = prepare_stan_data_inputs(x),
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        chains = chains,
        ...
      )
    } else {
      suppressMessages(
        x@model$sample(
          data = prepare_stan_data_inputs(x),
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

#' Sample from Stan model contained in `Simulation` object
#'
#' @param posterior_quantiles numeric vector of length two.
#' The posterior quantiles used for summarizing simulation results. The
#' default is `c(0.025, 0.975)` See details.
#' @param iter_warmup integer. The number of warm up iterations to run per chain.
#' The default is 1000.
#' @param iter_sampling integer. The number of post-warm up iterations to run per chain.
#' The default is 10000.
#' @param chains integer. The number of Markov chains to run. The default is 4.
#' @param keep_cmd_stan_models logical. Whether to keep the
#' `CmdStanModel` objects from the `mcmc_sampler` (`TRUE`,
#' discouraged in most scenarios) or not (`FALSE`). The default is `FALSE`.
#' @param verbose logical. Whether to print sampler updates (`TRUE`) or not
#' (`FALSE`)
#'
#' @include create_simulation_obj.R
#'
#' @rdname mcmc_sample
#' @return An object of class `MCMCSimulationResult`
#'
#' @details
#' ## Simulation objects
#' This function takes draws from an MCMC sampler and summarizes
#' results. Several metrics are summarized:
#'
#' \itemize{
#'   \item{"Type 1 error"}{ This is the probability that the posterior
#'   treatment effect distribution excludes the true value.}
#' }
#'
#' @export
#'
#' @examples
#' ## Simulation objects
#' base_mat <- matrix(
#'   c(
#'     rep(0, 200), rep(0, 200), rep(1, 200),
#'     rep(1, 200), rep(0, 200), rep(0, 200),
#'     rep(0, 600)
#'   ),
#'   ncol = 3,
#'   dimnames = list(NULL, c("ext", "trt", "driftOR"))
#' )
#'
#' add_binary_endpoint <- function(odds_ratio,
#'                                 base_matrix = base_mat) {
#'   linear_predictor <- base_matrix[, "trt"] * log(odds_ratio)
#'   prob <- 1 / (1 + exp(-linear_predictor))
#'
#'   bin_endpoint <- rbinom(
#'     NROW(base_matrix),
#'     1,
#'     prob
#'   )
#'
#'   cbind(base_matrix, matrix(bin_endpoint, ncol = 1, dimnames = list(NULL, "ep")))
#' }
#'
#' data_list <- list(
#'   list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
#'   list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
#' )
#'
#' guide <- data.frame(
#'   trueOR = c(1.5, 2.5),
#'   driftOR = c(1.0, 1.0),
#'   index = 1:2
#' )
#'
#' sdl <- sim_data_list(
#'   data_list = data_list,
#'   guide = guide,
#'   effect = "trueOR",
#'   drift = "driftOR",
#'   index = "index"
#' )
#'
#' sim_object <- create_simulation_obj(
#'   data_matrix_list = sdl,
#'   outcome = logistic_bin_outcome("ep", normal_prior(0, 1000)),
#'   borrowing = sim_borrowing_list(list(
#'     full_borrowing = borrowing_details("Full borrowing", "ext"),
#'     bdb = borrowing_details("BDB_HCP", "ext", exponential_prior(0.0001))
#'   )),
#'   treatment = treatment_details("trt", normal_prior(0, 1000))
#' )
#'
#' mcmc_sample(sim_object, chains = 1, iter_warmup = 500L, iter_sampling = 1000L)
setMethod(
  "mcmc_sample",
  signature = "Simulation",
  definition = function(x,
                        posterior_quantiles = c(0.025, 0.975),
                        iter_warmup = 1000L,
                        iter_sampling = 10000L,
                        chains = 4L,
                        verbose = FALSE,
                        keep_cmd_stan_models = FALSE,
                        ...) {
    # Input checks
    assert_class(x, "Simulation")
    assert_numeric(posterior_quantiles)
    assert_vector(posterior_quantiles, len = 2)
    assert_numeric(iter_warmup)
    assert_numeric(iter_sampling)
    assert_numeric(chains)
    assert_flag(verbose)
    assert_flag(keep_cmd_stan_models)
    if (any(posterior_quantiles < 0 | posterior_quantiles > 1)) {
      stop("`posterior_quantiles` must be [0, 1]")
    }

    # Message if `keep_cmd_stan_models` = `TRUE`
    if (keep_cmd_stan_models) {
      message("Creating ", x@n_analyses, " `draws` objects! ")
      if (x@n_analyses > 20) {
        message("This is a lot! You may consider `keep_cmd_stan_models` = `FALSE`.")
      }
      cmd_stan_models_out <- list()
    }

    # Create object
    mcmc_simulation_result <- .mcmc_simulation_result(
      results = x@guide
    )
    mcmc_simulation_result@results$true_coverage <-
      mcmc_simulation_result@results$null_coverage <-
      mcmc_simulation_result@results$bias_mean <-
      mcmc_simulation_result@results$mse_mean <-
      rep(NA_real_, x@n_combos)

    # MCMC sample
    for (i in 1:x@n_combos) {
      # Create a list for `cmd_stan_models`
      if (keep_cmd_stan_models) {
        cmd_stan_models_out[[i]] <- list()
      }

      # Important values
      n_sim <- NROW(x@analysis_obj_list[[i]])
      true_effect <- x@guide[i, x@data_matrix_list@effect]

      # Placeholder objects
      true_coverage <- null_coverage <- vector(mode = "integer", length = n_sim)
      bias <- mse <- vector(mode = "numeric", length = n_sim)

      for (j in 1:n_sim) {
        anls_obj <- x@analysis_obj_list[[i]][[j]]

        mcmc_results <- mcmc_sample(
          anls_obj,
          iter_warmup = iter_warmup,
          iter_sampling = iter_sampling,
          chains = chains,
          verbose = verbose,
          ...
        )

        draws <- mcmc_results$draws()

        # Coverage
        true_coverage[j] <- sim_is_true_effect_covered(
          draws,
          true_effect,
          posterior_quantiles
        )

        null_coverage[j] <- sim_is_null_effect_covered(
          draws,
          posterior_quantiles
        )

        # Bias
        bias[j] <- sim_estimate_bias(
          draws,
          true_effect
        )

        # MSE
        mse[j] <- sim_estimate_mse(
          draws,
          true_effect
        )

        # Save draws if desired
        if (keep_cmd_stan_models) {
          cmd_stan_models_out[[i]][[j]] <- mcmc_results
        } else {
          for (file in mcmc_results$output_files()) {
            file.remove(file)
          }
          rm(mcmc_results)
        }
      }

      # Add simulation study results
      mcmc_simulation_result@results$true_coverage[[i]] <- mean(true_coverage)
      mcmc_simulation_result@results$null_coverage[[i]] <- mean(null_coverage)
      mcmc_simulation_result@results$bias_mean[[i]] <- mean(bias)
      mcmc_simulation_result@results$mse_mean[[i]] <- mean(mse)
    }

    # Attach draws
    if (keep_cmd_stan_models) {
      mcmc_simulation_result@cmd_stan_models <- cmd_stan_models_out
    }

    # Return object
    return(mcmc_simulation_result)
  }
)
