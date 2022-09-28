#' Create an Example Analysis Object
#'
#' @param example integer. Which example object to create:
#'  * 1: Dynamic borrowing with exponential survival and two covariates.
#'  * 2: Dynamic borrowing with Weibull survival and two covariates.
#'
#' @return An `Analysis` object
#' @export
#'
#' @examples
#' example_analysis_object(1)
example_analysis_object <- function(example = 1) {
  check_choice(example, c(13))

  if (example == 1) {
    # Object with Exp survival, BDB, 2 covariates
    result <- create_analysis_obj(
      data_matrix = example_matrix,
      covariates = add_covariates(
        covariates = c("cov1", "cov2"),
        priors = normal_prior(0, 1000)
      ),
      outcome = exp_surv_dist(
        "time",
        "cnsr",
        baseline_prior = normal_prior(0, 1000)
      ),
      borrowing = borrowing_details(
        "BDB",
        "ext",
        exponential_prior(.001)
      ),
      treatment = treatment_details(
        "trt",
        normal_prior(0, 1000)
      )
    )
  } else if (example == 2) {
    # Object with Weibull survival, BDB, 2 covariates
    result <- create_analysis_obj(
      data_matrix = example_matrix,
      covariates = add_covariates(
        covariates = c("cov1", "cov2"),
        priors = normal_prior(0, 1000)
      ),
      outcome = weib_ph_surv_dist(
        "time",
        "cnsr",
        baseline_prior = normal_prior(0, 1000),
        shape_prior = exponential_prior(beta = 0.0001)
      ),
      borrowing = borrowing_details(
        "BDB",
        "ext",
        exponential_prior(.001)
      ),
      treatment = treatment_details(
        "trt",
        normal_prior(0, 1000)
      )
    )
  }

  result
}
