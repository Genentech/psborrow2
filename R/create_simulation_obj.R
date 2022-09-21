#' Compile MCMC sampler using STAN and create simulation object
#'
#' @param data_list `SimDataList`. The list of lists of data matrices created
#' with `sim_data_list()`.
#' @param outcome_list `SimOutcomeList` or `Outcome`. List of `Outcome` objects created with
#' `sim_outcome_list()`, or single `Outcome` object (e.g., created by `exp_surv_dist()`).
#' @param borrowing_list `SimBorrowingList` or `Borrowing`. List of `Borrowing` objects created
#' with `sim_borrowing_list()`, or a single `Borrowing` object created by `borrowing_details()`.
#' @param covariate_list `SimCovariateList` or `Covariate` or `NULL`. List of `Covariate` objects created
#' with `sim_covariate_list()`, a single `Covariate` object created by `add_covariates()`,
#' or `NULL` (no covariate adjustment).
#' @param treatment_list `SimTreatmentList` or `Treatment`. List of `Treatment` objects created
#' with `sim_treatment_list()` or a single `Treatment` object created by `treatment_details()`.
#' @param quiet logical. Whether to print messages (`quiet = FALSE`) or not
#' (`quiet = TRUE`, the default)
#' @return Object of class [`Simulation`][Simulation-class].
#'
#' @include simulation_class.R
#'
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
#' sim_object <- create_simulation_obj(
#'   data_list = sdl,
#'   outcome_list = logistic_bin_outcome("ep", normal_prior(0, 1000)),
#'   borrowing_list = sim_borrowing_list(list(
#'     full_borrowing = borrowing_details("Full borrowing", "ext"),
#'     bdb = borrowing_details("BDB", "ext", exponential_prior(0.0001))
#'   )),
#'   treatment_list = treatment_details("trt", normal_prior(0, 1000))
#' )
#' @export
create_simulation_obj <- function(data_list,
                                  covariate_list = NULL,
                                  outcome_list,
                                  borrowing_list,
                                  treatment_list,
                                  quiet = TRUE) {
  # Check inputs
  assert_class(data_list, "SimDataList")
  assert_multi_class(covariate_list, c("SimCovariateList", "Covariates", "NULL"))
  assert_multi_class(outcome_list, c("SimOutcomeList", "Outcome"))
  assert_multi_class(borrowing_list, c("SimBorrowingList", "Borrowing"))
  assert_multi_class(treatment_list, c("SimTreatmentList", "Treatment"))

  # Create empty covariate list if NULL
  if (is.null(covariate_list)) {
    covariate_list <- sim_covariate_list(covariate_list = list(`No adjustment` = NULL))
  }

  # Create lists for non-list objects
  if (is(covariate_list, "Covariates")) {
    covariate_list <- sim_covariate_list(covariate_list = list(default = covariate_list))
  }
  if (is(outcome_list, "Outcome")) {
    outcome_list <- sim_outcome_list(outcome_list = list(default = outcome_list))
  }
  if (is(borrowing_list, "Borrowing")) {
    borrowing_list <- sim_borrowing_list(borrowing_list = list(default = borrowing_list))
  }
  if (is(treatment_list, "Treatment")) {
    treatment_list <- sim_treatment_list(treatment_list = list(default = treatment_list))
  }

  # Create object
  simulation_obj <- .simulation_obj(
    data_list = data_list,
    covariate_list = covariate_list,
    outcome_list = outcome_list,
    borrowing_list = borrowing_list,
    treatment_list = treatment_list
  )

  # Check that inputs are consistent with each other
  ## Data matrices all have external flags, treatment flags, and covariates
  search_cols <- get_vars(simulation_obj)

  for (i in seq_along(simulation_obj@data_list@data_list)) {
    for (j in seq_along(simulation_obj@data_list@data_list[[i]])) {
      if (!all(search_cols %in% colnames(simulation_obj@data_list@data_list[[i]][[j]]))) {
        which_not_in <- search_cols[which(!search_cols %in% colnames(simulation_obj@data_list@data_list[[i]][[j]]))]
        stop(
          "The following columns were specified in the simulation but ",
          "are missing in some simulated data matrices: '",
          paste0(which_not_in, collapse = "', '"),
          "'"
        )
      }
    }
  }

  ## Data matrices do not contain missing data
  for (i in seq_along(simulation_obj@data_list@data_list)) {
    for (j in seq_along(simulation_obj@data_list@data_list[[i]])) {
      mat_subset <- simulation_obj@data_list@data_list[[i]][[j]][, search_cols]
      if (!all(complete.cases(mat_subset))) {
        stop(
          "Missing data detected in >1 matrix in `data_list`. ",
          "Could be one of the following columns: '",
          paste0(search_cols, collapse = "', '"),
          "'. ",
          "Error found in simulation_obj@data_list@data_list[[", i, "]][[", j, "]]"
        )
      }
    }
  }

  # Create guide
  simulation_obj@guide <- Reduce(
    merge,
    init = simulation_obj@data_list@guide,
    x = list(
      simulation_obj@outcome_list@guide,
      simulation_obj@borrowing_list@guide,
      simulation_obj@covariate_list@guide,
      simulation_obj@treatment_list@guide
    )
  )
  simulation_obj@n_combos <- NROW(simulation_obj@guide)
  simulation_obj@n_analyses <- sum(simulation_obj@guide$n_datasets_per_param)

  # Create analysis objects
  simulation_obj@analysis_obj_list <- make_analysis_object_list(simulation_obj,
    quiet = quiet
  )

  # Return simulation object
  return(simulation_obj)
}
