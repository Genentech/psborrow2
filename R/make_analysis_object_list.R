#' Make list of analysis objects for class `Simulation`
#'
#' @param simulation_obj `Simulation`. Object of class [`Simulation`][Simulation-class]
#' created by `psborrow2:::.simulation_obj()`.
#' @param quiet logical. Whether to print messages (`quiet = FALSE`) or not
#' (`quiet = TRUE`, the default)
#' @return list of lists of `Analysis` objects
#'
#' @include create_simulation_obj.R
#' @noRd
#' @examples
#'
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
#' sim_object <- psborrow2:::.simulation_obj(
#'   data_matrix_list = sdl,
#'   outcome = sim_outcome_list(list(default = outcome_bin_logistic("ep", prior_normal(0, 1000)))),
#'   covariate = sim_covariate_list(covariate_list = list(`No adjustment` = NULL)),
#'   borrowing = sim_borrowing_list(list(
#'     full_borrowing = borrowing_full("ext"),
#'     bdb = borrowing_hierarchical_commensurate("ext", prior_exponential(0.0001))
#'   )),
#'   treatment = sim_treatment_list(list(default = treatment_details("trt", prior_normal(0, 1000))))
#' )
#'
#' sim_object@guide <- Reduce(
#'   merge,
#'   init = sim_object@data_matrix_list@guide,
#'   x = list(
#'     sim_object@outcome@guide,
#'     sim_object@borrowing@guide,
#'     sim_object@covariate@guide,
#'     sim_object@treatment@guide
#'   )
#' )
#'
#' sim_object@n_combos <- NROW(sim_object@guide)
#' sim_object@n_analyses <- sum(sim_object@guide$n_datasets_per_param)
#'
#' psborrow2:::make_analysis_object_list(sim_object)
make_analysis_object_list <- function(simulation_obj,
                                      quiet = TRUE) {
  # Create top-level empty list
  analysis_obj_list <- vector("list", simulation_obj@n_combos)

  # Loop over top level in guide
  for (i in seq_len(simulation_obj@n_combos)) {
    guide_row <- simulation_obj@guide[i, ]

    # Objects needed for `create_analysis_obj()`
    covariates <- simulation_obj@covariate@covariate_list[[
      guide_row[["covariate_scenario"]]
    ]]

    outcome <- simulation_obj@outcome@outcome_list[[
      guide_row[["outcome_scenario"]]
    ]]

    borrowing <- simulation_obj@borrowing@borrowing_list[[
      guide_row[["borrowing_scenario"]]
    ]]

    treatment <- simulation_obj@treatment@treatment_list[[
      guide_row[["treatment_scenario"]]
    ]]

    data_matrix_list <- simulation_obj@data_matrix_list@data_list[[
      guide_row[[simulation_obj@data_matrix_list@index]]
    ]]

    analysis_obj_list[[i]] <- lapply(
      data_matrix_list,
      FUN = create_analysis_obj,
      covariates = covariates,
      outcome = outcome,
      borrowing = borrowing,
      treatment = treatment,
      quiet = quiet
    )
  }

  # Return simulation object
  return(analysis_obj_list)
}
