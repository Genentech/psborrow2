#' Specify Treatment Details
#'
#' Specify the treatment arm column name in the model matrix and set a prior
#' distribution for the treatment effect (log hazard ratio or
#' log odds ratio)
#'
#' @param trt_flag_col character. The name of the column
#' in the model matrix that corresponds to the treatment flag
#' (`1`/`0` or `TRUE`/`FALSE`). This identifies patients as belonging
#' to the experimental treatment arm.
#' @param trt_prior Object of class `Prior` specifying the
#' prior distribution of the log effect estimate (log hazard ratio for
#' time to event endpoints and log odds ratio for binary endpoints).
#'
#' @return Object of class [`Treatment`][Treatment-class].
#' @export
#'
#' @include treatment_class.R
#'
#' @examples
#' sta <- treatment_details(
#'   trt_flag_col = "trt",
#'   trt_prior = prior_normal(0, 1000)
#' )
treatment_details <- function(trt_flag_col,
                              trt_prior) {
  # Additional checks and neater errors that in class definition
  assert_string(trt_flag_col)
  assert_class(trt_prior, "Prior")

  .treatment_class(
    trt_flag_col = trt_flag_col,
    trt_prior = trt_prior
  )
}
