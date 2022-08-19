#' Plot Probability Density Function Values
#'
#' @param x values
#' @param y probability density values `y = f(x)`
#' @param ... passed to [plot()]
#'
#' Plots the density values as a curve with the lower vertical limit set to 0.
#'
#' @export
#'
#' @examples
#' x <- seq(-2, 2, len = 100)
#' y <- dnorm(x)
#' plot_pdf(x, y)
plot_pdf <- function(x, y, ...) {
  plot(x,
    y,
    type = "l",
    ylim = c(0, max(y[is.finite(y)])),
    ylab = "density",
    ...
  )
}

#' Plot Probability Mass Function Values
#'
#' @param x values
#' @param y probability mass values `y = f(x)`
#' @param ... passed to [plot()] and [rect()]
#' @param col Fill color of bars.
#' @param add Add bars to existing plot.
#' @param xlim Limits of x axis.
#'
#' Plots the probability values as a barplot.
#'
#' @export
#'
#' @examples
#' x <- seq(0, 5)
#' y <- dpois(x, lambda = 2)
#' plot_pmf(x, y)
plot_pmf <- function(x, y, ..., col = "grey", add = FALSE, xlim) {
  if (isFALSE(add)) {
    xlim <- range(x) + c(-0.5, 0.5)
    ylim <- c(0, max(y))
    plot(x, y, type = "n", xaxt = "n", xlab = "", ylab = "", ..., xlim = xlim, ylim = ylim)
    graphics::axis(side = 1, at = x, col.ticks = NA)
  }
  graphics::rect(x - 0.5, 0, x + 0.5, y, col = rep(col, length(y)), ...)
}

#' Glue Strings with Default Arguments
#'
#' Calls [glue::glue()] with `.open = '{{'` and `.close = '}}'`
#' to simplify gluing Stan code.
#'
#' @param ... Arguments passed to [glue::glue()]
#'
#' @return A string.
#' @noRd
#' @examples
#' name <- "Tom"
#' psborrow2:::h_glue("hello, my name is {{name}}.")
h_glue <- function(...) {
  glue::glue(..., .open = "{{", .close = "}}", .envir = parent.frame())
}


#' Check Data Matrix for Required Columns
#'
#' Check that an `Analysis` object's `data_matrix` has all the required variables.
#'
#' @param object `Analysis`. Object to check.
#'
#' @return `stop()` if some columns are missing.
#' @export
#' @examples
#' data_matrix <- structure(c(
#'   1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
#'   1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
#'   1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,
#'   0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1,
#'   0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1,
#'   1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1,
#'   1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0,
#'   1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 5.32295081977934,
#'   6.96715560527452, 1.17501259866481, 9.45936763681621, 5.75572041253912,
#'   12.1139661284359, 2.64741266341488, 4.99828513121648, 5.38734198746281,
#'   4.74770899862051, 0.0803900761309156, 13.7720370325053, 3.03310634382069,
#'   10.1695853577489, 0.0720591936260462, 10.1367262049345, 2.9709762107209,
#'   0.659847613424063, 3.88436722227683, 3.2750634373027, 1.90838416890977,
#'   5.79706331825161, 4.28611800974856, 0.702194716266679, 4.74582234003252,
#'   6.92417557015123, 6.53942201171797, 5.88460493011677, 1.84311583921956,
#'   5.28505285794622, 4.34498298102206, 3.17685930818209, 11.0179639531233,
#'   2.14560192144267, 4.40741405311895, 10.9576044368026, 3.55944875309522,
#'   9.07620135719862, 1.29542022943497, 3.35630633204141, 14.1141011930051,
#'   14.3560852138326, 6.76962562138734, 6.60672739803918, 0.727092696356863,
#'   3.06457582335024, 2.27240795704226, 6.12868075434827, 7.45796004200603,
#'   9.23882804838511, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
#'   0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1,
#'   1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0
#' ), dim = c(50L, 6L), dimnames = list(
#'   NULL, c("ext", "trt", "cov1", "cov2", "time", "cnsr")
#' ))
#'
#' anls <- psborrow2:::.analysis_obj(
#'   data_matrix = data_matrix,
#'   covariates = add_covariates(
#'     covariates = c("cov1", "cov2"),
#'     priors = normal_prior(0, 1000)
#'   ),
#'   outcome = exp_surv_dist(
#'     "time",
#'     "cnsr"
#'   ),
#'   borrowing = borrowing_details(
#'     "BDB",
#'     "ext",
#'     exponential_prior(.001),
#'     baseline_prior = normal_prior(0, 1000)
#'   ),
#'   treatment = treatment_details(
#'     "trt",
#'     normal_prior(0, 1000)
#'   ),
#'   ready_to_sample = FALSE
#' )
#'
#' check_data_matrix_has_columns(anls)
#'
check_data_matrix_has_columns <- function(object) {
  assert_class(object, "Analysis")
  data_cols <- colnames(object@data_matrix)
  error_col <- c()

  if (!object@treatment@trt_flag_col %in% data_cols) {
    error_col <- c(error_col, treatment = object@treatment@trt_flag_col)
  }

  if (!object@borrowing@ext_flag_col %in% data_cols) {
    error_col <- c(error_col, borrowing = object@borrowing@ext_flag_col)
  }

  if (!is.null(object@covariates)) {
    missing_covariates <- setdiff(object@covariates@covariates, data_cols)
    if (length(missing_covariates)) error_col <- c(error_col, covariates = toString(missing_covariates))
  }

  if (is(object@outcome, "TimeToEvent")) {
    missing_outcomes <- setdiff(c(object@outcome@time_var, object@outcome@cens_var), data_cols)
    if (length(missing_outcomes)) error_col <- c(error_col, outcome = toString(missing_outcomes))
  } else if (is(object@outcome, "BinaryOutcome")) {
    if (!object@outcome@binary_var %in% data_cols) error_col <- c(error_col, outcome = object@outcome@binary_var)
  }

  if (length(error_col)) {
    stop(
      "The following specified variables were not found in `data_matrix`:\n",
      paste0("  ", names(error_col), ": ", error_col, "\n")
    )
  }
}
