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
#' if (check_cmdstan()) {
#'   anls <- create_analysis_obj(
#'     data_matrix = example_matrix,
#'     covariates = add_covariates(
#'       covariates = c("cov1", "cov2"),
#'       priors = prior_normal(0, 1000)
#'     ),
#'     outcome = outcome_surv_weibull_ph(
#'       "time",
#'       "cnsr",
#'       shape_prior = prior_normal(0, 1000),
#'       baseline_prior = prior_normal(0, 1000)
#'     ),
#'     borrowing = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_exponential(.001)
#'     ),
#'     treatment = treatment_details("trt", prior_normal(0, 1000))
#'   )
#'
#'   mcmc_results <- mcmc_sample(anls, chains = 1, iter_warmup = 500L, iter_sampling = 1000L)
#' }
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

    if (!isTRUE(x@ready_to_sample)) {
      stop(
        "Cannot sample object. Check cmdstanr is installed and create object using `create_analysis_obj()`",
        call. = FALSE
      )
    }

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
#' results.
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
#' if (check_cmdstan()) {
#'   sim_object <- create_simulation_obj(
#'     data_matrix_list = sdl,
#'     outcome = outcome_bin_logistic("ep", prior_normal(0, 1000)),
#'     borrowing = sim_borrowing_list(list(
#'       full_borrowing = borrowing_full("ext"),
#'       bdb = borrowing_hierarchical_commensurate("ext", prior_exponential(0.0001))
#'     )),
#'     treatment = treatment_details("trt", prior_normal(0, 1000))
#'   )
#'
#'   mcmc_sample(sim_object, chains = 1, iter_warmup = 500L, iter_sampling = 1000L)
#' }
#' \dontrun{
#' library(future)
#' # Use two separate R processes
#' plan("multisession", workers = 2)
#'
#' # and two parallel threads in each.
#' mcmc_sample(sim_object, chains = 1, iter_warmup = 500L, iter_sampling = 1000L, parallel_chains = 2)
#'
#' # Tidy up processes when finished
#' plan("sequential")
#' }
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
    if (!is_cmdstanr_available()) {
      stop(
        "`mcmc_sample()` failed because `cmdstanr` is required for sampling. ",
        "Install `cmdstanr` and create another simulation object with `create_simulation_obj()`, ",
        "then call `mcmc_sample()` again.\n",
        "To install:\n",
        "install.packages(\"cmdstanr\", repos = c(\"https://mc-stan.org/r-packages/\", getOption(\"repos\")))"
      )
    }
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
      mcmc_simulation_result@results$trt_var <-
      vector("list", x@n_combos)

    parent_cmdstanr_path <- cmdstanr::cmdstan_path()

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
      bias <- mse <- var <- vector(mode = "numeric", length = n_sim)

      sim_futures <- list()
      for (j in 1:n_sim) {
        anls_obj <- x@analysis_obj_list[[i]][[j]]
        if (!anls_obj@ready_to_sample) {
          warning(
            "Model x@analysis_obj_list[[", i, "]][[", j, "]]) is not ready ",
            "for sampling and unexpected results may occur. ",
            "Please check cmdstanr is installed and recreate objects.",
            immediate. = TRUE,
            call. = FALSE
          )
          sim_futures[[j]] <- future(expr = list(
            true_coverage = NA,
            null_coverage = NA,
            bias = NA,
            mse = NA,
            var = NA
          ))
        } else {
          sim_futures[[j]] <- future(
            packages = "psborrow2",
            seed = TRUE,
            expr = {
              if (!check_cmdstan()) cmdstanr::set_cmdstan_path(parent_cmdstanr_path)
              if (!file.exists(anls_obj@model$exe_file())) anls_obj@model$compile()

              mcmc_results <- mcmc_sample(
                anls_obj,
                iter_warmup = iter_warmup,
                iter_sampling = iter_sampling,
                chains = chains,
                verbose = verbose,
                ...
              )

              draws <- mcmc_results$draws()

              keep <- list()
              # Coverage
              keep$true_coverage <- psborrow2:::sim_is_true_effect_covered(
                draws,
                true_effect,
                posterior_quantiles
              )

              keep$null_coverage <- psborrow2:::sim_is_null_effect_covered(
                draws,
                posterior_quantiles
              )

              # Bias
              keep$bias <- psborrow2:::sim_estimate_bias(
                draws,
                true_effect
              )

              # MSE
              keep$mse <- psborrow2:::sim_estimate_mse(
                draws,
                true_effect
              )

              # Variance of beta_trt
              keep$var <- psborrow2:::sim_estimate_effect_variance(draws)

              # Save draws if desired
              if (keep_cmd_stan_models) {
                keep$cmd_stan_models_out <- mcmc_results
              } else {
                for (file in mcmc_results$output_files()) {
                  file.remove(file)
                }
                rm(mcmc_results)
              }
              keep
            }
          )
        }
      }

      for (j in 1:n_sim) {
        sim_result <- value(sim_futures[[j]])
        true_coverage[j] <- sim_result$true_coverage
        null_coverage[j] <- sim_result$null_coverage
        bias[j] <- sim_result$bias
        mse[j] <- sim_result$mse
        var[j] <- sim_result$var
        if (keep_cmd_stan_models) cmd_stan_models_out[[i]][[j]] <- sim_result$cmd_stan_models_out
      }

      # Add simulation study results
      mcmc_simulation_result@results$true_coverage[[i]] <- true_coverage
      mcmc_simulation_result@results$null_coverage[[i]] <- null_coverage
      mcmc_simulation_result@results$bias_mean[[i]] <- bias
      mcmc_simulation_result@results$mse_mean[[i]] <- mse
      mcmc_simulation_result@results$trt_var[[i]] <- var
    }

    # Attach draws
    if (keep_cmd_stan_models) {
      mcmc_simulation_result@cmd_stan_models <- cmd_stan_models_out
    }

    # Return object
    return(mcmc_simulation_result)
  }
)
