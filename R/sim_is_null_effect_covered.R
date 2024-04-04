#' Do the posterior quantiles contain a value of 1.0?
#'
#' @param draws draws_array Object of class `draws` from
#' `CmdStanMCMC$draws()`.
#' @param posterior_quantiles numeric. Vector of length two specifying
#' quantiles of the posterior treatment effect distribution in which
#' to search for the null effect.
#'
#' @return 1L if the null effect (1.0) is contained within the quantiles, else 0L
#' @noRd
#' @examples
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
#' x <- create_simulation_obj(
#'   data_matrix_list = sdl,
#'   outcome = outcome_bin_logistic("ep", prior_normal(0, 1000)),
#'   borrowing = sim_borrowing_list(list(
#'     full_borrowing = borrowing_full("ext"),
#'     bdb = borrowing_hierarchical_commensurate("ext", prior_exponential(0.0001))
#'   )),
#'   treatment = treatment_details("trt", prior_normal(0, 1000))
#' )
#'
#' \donttest{
#' i <- 1
#' j <- 1
#' anls_obj <- x@analysis_obj_list[[i]][[j]]
#' res <- mcmc_sample(anls_obj, iter_sampling = 500)
#' draws <- res$draws()
#'
#' psborrow2:::sim_is_null_effect_covered(
#'   draws,
#'   c(0.025, 0.975)
#' )
#' }
sim_is_null_effect_covered <- function(draws,
                                       posterior_quantiles) {
  summ_draws <- posterior::summarise_draws(draws, ~ quantile(.x, probs = posterior_quantiles))
  effect_range <- c(
    summ_draws[summ_draws$variable %in% c("HR_trt", "OR_trt"), 2][[1]],
    summ_draws[summ_draws$variable %in% c("HR_trt", "OR_trt"), 3][[1]]
  )
  return(as.integer(1.00 >= effect_range[1] & 1.00 <= effect_range[2]))
}
