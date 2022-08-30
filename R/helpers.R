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


#' Rename Covariates in `draws` Object
#'
#' @param analysis `Analysis` as created by [`create_analysis_obj()`].
#' @param draws `draws` created from sampled analysis object. See example.
#'
#' @return A `draws`[[posterior::draws]] object with covariate names.
#' @export
#'
#' @examples
#' analysis_object <- create_analysis_obj(
#'   data_matrix = example_matrix,
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
#'   )
#' )
#' samples <- mcmc_sample(analysis_object)
#' draws <- samples$draws()
#' renamed_draws <- rename_draws_covariates(draws, analysis_object)
#' summary(renamed_draws)
rename_draws_covariates <- function(draws, analysis) {
  assert_class(draws, "draws")
  assert_class(analysis, "Analysis")
  covariates <- paste0("b_", get_vars(analysis@covariates))
  names <- stats::setNames(paste0("beta[", seq_along(covariates), "]"), covariates)
  do.call(posterior::rename_variables, args = c(list(.x = draws), as.list(names)))
}


#' Create Variable Dictionary
#'
#' @param analysis_obj `Analysis`. Object to describe variable names.
#'
#' @return A `data.frame` with the names of Stan variables and the descriptions.
#' @export
variable_dictionary <- function(analysis_obj) {
  assert_class(analysis_obj, "Analysis")
  is_tte <- isTRUE(inherits(analysis_obj@outcome, "TimeToEvent"))
  is_bdb <- isTRUE(analysis_obj@borrowing@method == "BDB")
  is_weib <- is_tte && isTRUE(inherits(analysis_obj@outcome, "WeibullPHSurvDist"))
  has_covs <- !is.null(analysis_obj@covariates)

  covariates <- if (has_covs) {
    covs <- get_vars(analysis_obj@covariates)
    stats::setNames(h_glue("beta[{{seq_along(covs)}}]"), covs)
  } else {
    NULL
  }

  if (is_tte) {
    beta_trt <- c("treatment log HR" = "beta_trt")
    exp_trt <- c("treatment HR" = "exp_trt")
    alpha_type <- "baseline log hazard rate"
    if (is_weib) {
      addl_params <- c("Weibull shape parameter" = "shape_weibull")
    } else {
      addl_params <- NULL
    }
  } else {
    beta_trt <- c("treatment log OR" = "beta_trt")
    exp_trt <- c("treatment OR" = "exp_trt")
    alpha_type <- "intercept"
    addl_params <- NULL
  }

  if (is_bdb) {
    alpha <- stats::setNames(c("alpha[1]", "alpha[2]"), paste0(alpha_type, c(", internal", ", external")))
    tau <- c("commensurability parameter" = "tau")
  } else {
    alpha <- setNames("alpha", alpha_type)
    tau <- NULL
  }

  vars <- c(tau, alpha, covariates, beta_trt, exp_trt, addl_params)
  data.frame(Stan_variable = unname(vars), Description = names(vars))
}
