#' Compile MCMC sampler using STAN and create simulation object
#'
#' @param data_list `SimDataList`. The list of lists of data matrices created
#' with `sim_data_list()`.
#' @param outcome_list `SimOutcomeList`. List of outcome objects created with
#' `sim_outcome_list()`.
#' @param borrowing_list `SimBorrowingList`. List of borrowing objects created
#' with `sim_borrowing_list()`.
#' @param covariate_list `SimCovariateList`. List of covariate objects created
#' with `sim_covariate_list()`.
#' @param treatment_list `SimTreatmentList`. List of treatment objects created
#' with `sim_treatment_list()`.
#'
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
#'   driftOR = c(1.0, 1.0)
#' )
#'
#' sdl <- sim_data_list(
#'   data_list = data_list,
#'   guide = guide,
#'   effect = "trueOR",
#'   drift = "driftOR"
#' )
#'
#' sim_object <- create_simulation_obj(
#'    data_list = sdl,
#'    outcome_list = sim_outcome_list(list(standard = logistic_bin_outcome("ep", normal_prior(0, 1000)))),
#'    borrowing_list = sim_borrowing_list(list(standard = borrowing_details("BDB", "ext", exponential_prior(0.0001)))),
#'    treatment_list = sim_treatment_list(list(standard = treatment_details("trt", normal_prior(0, 1000))))
#'    )
#' @export
create_simulation_obj <- function(data_list,
                                  covariate_list = NULL,
                                  outcome_list,
                                  borrowing_list,
                                  treatment_list) {
  # Check inputs
  assert_class(data_list, "SimDataList")
  assert_multi_class(covariate_list, c("SimCovariateList", "NULL"))
  assert_class(outcome_list, "SimOutcomeList")
  assert_class(borrowing_list, "SimBorrowingList")
  assert_class(treatment_list, "SimTreatmentList")

  # Create object
  simulation_obj <- .simulation_obj(
    data_list = data_list,
    covariate_list =  covariate_list,
    outcome_list = outcome_list,
    borrowing_list = borrowing_list,
    treatment_list = treatment_list
  )

  # Check that inputs are consistent with each other
  ## Data matrices all have external flags, treatment flags, and covariates
  search_cols <- get_vars(simulation_obj)

  for (i in 1:NROW(simulation_obj@data_list@data_list)) {
    for (j in 1:NROW(simulation_obj@data_list@data_list[[i]])) {
      if (!all(search_cols %in% colnames(simulation_obj@data_list@data_list[[i]][[j]]))) {
        which_not_in <- search_cols[which(!search_cols %in% colnames(simulation_obj@data_list@data_list[[i]][[j]]))]
        stop("The following columns were specified in the simulation but are missing in some simulated data matrices: '",
             paste0(which_not_in, collapse = "', '"),
             "'")
      }
    }
  }

  ## Data matrices do not contain missing data
  for (i in 1:NROW(simulation_obj@data_list@data_list)) {
    for (j in 1:NROW(simulation_obj@data_list@data_list[[i]])) {
      mat_subset <- simulation_obj@data_list@data_list[[i]][[j]][,search_cols]
      if (!all(complete.cases(mat_subset))) {
        stop("Missing data detected in >1 matrix in `data_list`. ",
             "Could be one of the following columns: '",
             paste0(search_cols, collapse = "', '"),
             "'. ",
             "Error found in simulation_obj@data_list@data_list[[",i,"]][[",j,"]]"
        )
      }
    }
  }

  # Create guide

  # Return simulation object
  return(simulation_obj)

}
