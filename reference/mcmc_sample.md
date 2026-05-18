# Sample from Stan model

Method to sample from compiled Stan model and return a `CmdStanMCMC`
object with draws.

## Usage

``` r
mcmc_sample(x, ...)

# S4 method for class 'ANY'
mcmc_sample(x, ...)

# S4 method for class 'Analysis'
mcmc_sample(
  x,
  iter_warmup = 1000L,
  iter_sampling = 10000L,
  chains = 4L,
  verbose = FALSE,
  ...
)

# S4 method for class 'Simulation'
mcmc_sample(
  x,
  posterior_quantiles = c(0.025, 0.975),
  iter_warmup = 1000L,
  iter_sampling = 10000L,
  chains = 4L,
  verbose = FALSE,
  keep_cmd_stan_models = FALSE,
  ...
)
```

## Arguments

- x:

  object to sample, such as `Analysis` (created with
  [`create_analysis_obj()`](https://genentech.github.io/psborrow2/reference/create_analysis_obj.md))
  or `Simulation`.

- ...:

  additional arguments passed to the \$sample() method of a `cmdstanr`
  Stan model. See
  https://mc-stan.org/cmdstanr/reference/model-method-sample.html

- iter_warmup:

  integer. The number of warm up iterations to run per chain. The
  default is 1000.

- iter_sampling:

  integer. The number of post-warm up iterations to run per chain. The
  default is 10000.

- chains:

  integer. The number of Markov chains to run. The default is 4.

- verbose:

  logical. Whether to print sampler updates (`TRUE`) or not (`FALSE`)

- posterior_quantiles:

  numeric vector of length two. The posterior quantiles used for
  summarizing simulation results. The default is `c(0.025, 0.975)` See
  details.

- keep_cmd_stan_models:

  logical. Whether to keep the `CmdStanModel` objects from the
  `mcmc_sampler` (`TRUE`, discouraged in most scenarios) or not
  (`FALSE`). The default is `FALSE`.

## Value

An object of class `CmdStanMCMC`

An object of class `MCMCSimulationResult`

## Details

### Simulation objects

This function takes draws from an MCMC sampler and summarizes results.

## Examples

``` r
## Analysis objects
if (check_cmdstan()) {
  anls <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      covariates = c("cov1", "cov2"),
      priors = prior_normal(0, 1000)
    ),
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      shape_prior = prior_normal(0, 1000),
      baseline_prior = prior_normal(0, 1000)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      "ext",
      prior_exponential(.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  mcmc_results <- mcmc_sample(anls, chains = 1, iter_warmup = 500L, iter_sampling = 1000L)
}
#> Inputs look good.
#> Stan program compiled successfully!
#> Ready to go! Now call `mcmc_sample()`.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 2.7 seconds.

## Simulation objects
base_mat <- matrix(
  c(
    rep(0, 200), rep(0, 200), rep(1, 200),
    rep(1, 200), rep(0, 200), rep(0, 200),
    rep(0, 600)
  ),
  ncol = 3,
  dimnames = list(NULL, c("ext", "trt", "driftOR"))
)

add_binary_endpoint <- function(odds_ratio,
                                base_matrix = base_mat) {
  linear_predictor <- base_matrix[, "trt"] * log(odds_ratio)
  prob <- 1 / (1 + exp(-linear_predictor))

  bin_endpoint <- rbinom(
    NROW(base_matrix),
    1,
    prob
  )

  cbind(base_matrix, matrix(bin_endpoint, ncol = 1, dimnames = list(NULL, "ep")))
}

data_list <- list(
  list(add_binary_endpoint(1.5), add_binary_endpoint(1.5)),
  list(add_binary_endpoint(2.5), add_binary_endpoint(2.5))
)

guide <- data.frame(
  trueOR = c(1.5, 2.5),
  driftOR = c(1.0, 1.0),
  index = 1:2
)

sdl <- sim_data_list(
  data_list = data_list,
  guide = guide,
  effect = "trueOR",
  drift = "driftOR",
  index = "index"
)

if (check_cmdstan()) {
  sim_object <- create_simulation_obj(
    data_matrix_list = sdl,
    outcome = outcome_bin_logistic("ep", prior_normal(0, 1000)),
    borrowing = sim_borrowing_list(list(
      full_borrowing = borrowing_full("ext"),
      bdb = borrowing_hierarchical_commensurate("ext", prior_exponential(0.0001))
    )),
    treatment = treatment_details("trt", prior_normal(0, 1000))
  )

  mcmc_sample(sim_object, chains = 1, iter_warmup = 500L, iter_sampling = 1000L)
}
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 0.4 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 0.4 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 0.3 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 0.3 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 3.0 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 2.0 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 2.5 seconds.
#> Running MCMC with 1 chain...
#> 
#> Chain 1 finished in 2.3 seconds.
#> `MCMCSimulationResult` object.  Call `get_results()` to save outputs as a data.frame
if (FALSE) { # \dontrun{
library(future)
# Use two separate R processes
plan("multisession", workers = 2)

# and two parallel threads in each.
mcmc_sample(sim_object, chains = 1, iter_warmup = 500L, iter_sampling = 1000L, parallel_chains = 2)

# Tidy up processes when finished
plan("sequential")
} # }
```
