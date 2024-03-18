#' Compile MCMC sampler using STAN and create simulation object
#'
#' @param data_matrix_list `SimDataList`. The list of lists of data matrices created
#' with `sim_data_list()`.
#' @param covariate `SimCovariateList` or `Covariate` or `NULL`. List of `Covariate` objects created
#' with `sim_covariate()`, a single `Covariate` object created by `add_covariates()`,
#' or `NULL` (no covariate adjustment).
#' @param outcome `SimOutcomeList` or `Outcome`. List of `Outcome` objects created with
#' `sim_outcome()`, or single `Outcome` object (e.g., created by `outcome_surv_exponential()`).
#' @param borrowing `SimBorrowingList` or `Borrowing`. List of `Borrowing` objects created
#' with `sim_borrowing()`, or a single `Borrowing` object created by `borrowing_full()`,
#' `borrowing_none()`, or `borrowing_hierarchical_commensurate()`.
#' @param treatment `SimTreatmentList` or `Treatment`. List of `Treatment` objects created
#' with `sim_treatment()` or a single `Treatment` object created by `treatment_details()`.
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
#' }
#' @export
create_simulation_obj <- function(data_matrix_list,
                                  covariate = NULL,
                                  outcome,
                                  borrowing,
                                  treatment,
                                  quiet = TRUE) {
  # Check inputs
  assert_class(data_matrix_list, "SimDataList")
  assert_multi_class(covariate, c("SimCovariateList", "Covariates", "NULL"))
  assert_multi_class(outcome, c("SimOutcomeList", "Outcome"))
  assert_multi_class(borrowing, c("SimBorrowingList", "Borrowing"))
  assert_multi_class(treatment, c("SimTreatmentList", "Treatment"))

  # Create empty covariate list if NULL
  if (is.null(covariate)) {
    covariate <- sim_covariate_list(covariate_list = list(`No adjustment` = NULL))
  }

  # Create lists for non-list objects
  if (is(covariate, "Covariates")) {
    covariate <- sim_covariate_list(covariate_list = list(default = covariate))
  }
  if (is(outcome, "Outcome")) {
    outcome <- sim_outcome_list(outcome_list = list(default = outcome))
  }
  if (is(borrowing, "Borrowing")) {
    borrowing <- sim_borrowing_list(borrowing_list = list(default = borrowing))
  }
  if (is(treatment, "Treatment")) {
    treatment <- sim_treatment_list(treatment_list = list(default = treatment))
  }

  # Create object
  simulation_obj <- .simulation_obj(
    data_matrix_list = data_matrix_list,
    covariate = covariate,
    outcome = outcome,
    borrowing = borrowing,
    treatment = treatment
  )

  # Check that inputs are consistent with each other
  ## Data matrices all have external flags, treatment flags, and covariates
  search_cols <- get_vars(simulation_obj)

  for (i in seq_along(simulation_obj@data_matrix_list@data_list)) {
    for (j in seq_along(simulation_obj@data_matrix_list@data_list[[i]])) {
      if (!all(search_cols %in% colnames(simulation_obj@data_matrix_list@data_list[[i]][[j]]))) {
        which_not_in <- search_cols[
          which(!search_cols %in%
            colnames(simulation_obj@data_matrix_list@data_list[[i]][[j]]))
        ]
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
  for (i in seq_along(simulation_obj@data_matrix_list@data_list)) {
    for (j in seq_along(simulation_obj@data_matrix_list@data_list[[i]])) {
      mat_subset <- simulation_obj@data_matrix_list@data_list[[i]][[j]][, search_cols]
      if (!all(complete.cases(mat_subset))) {
        stop(
          "Missing data detected in >1 matrix in `data_matrix_list`. ",
          "Could be one of the following columns: '",
          paste0(search_cols, collapse = "', '"),
          "'. ",
          "Error found in simulation_obj@data_matrix_list@data_list[[", i, "]][[", j, "]]"
        )
      }
    }
  }

  # Create guide
  simulation_obj@guide <- Reduce(
    merge,
    init = simulation_obj@data_matrix_list@guide,
    x = list(
      simulation_obj@outcome@guide,
      simulation_obj@borrowing@guide,
      simulation_obj@covariate@guide,
      simulation_obj@treatment@guide
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
