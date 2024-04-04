#' Plot Probability Density Function Values
#'
#' @param x values
#' @param y probability density values `y = f(x)`
#' @param ... passed to [plot()]
#'
#' Plots the density values as a curve with the lower vertical limit set to 0.
#'
#' 
#' @return No return value, this function generates a plot in the current graphics device.
#'
#' @export
#' 
#' @examples
#' x <- seq(-2, 2, len = 100)
#' y <- dnorm(x)
#' plot_pdf(x, y)
plot_pdf <- function(x, y, ...) {
  pars_list <- list(
    x = x,
    y = y,
    type = "l",
    ylim = c(0, max(y[is.finite(y)])),
    ylab = "density",
    xlab = ""
  )
  for (i in ...names()) pars_list[[i]] <- list(...)[[i]]

  do.call(plot, pars_list)
}

#' Plot Probability Mass Function Values
#'
#' @param x values
#' @param y probability mass values `y = f(x)`
#' @param ... passed to [plot()] and [rect()]
#' @param col Fill color of bars.
#' @param add Add bars to existing plot.
#'
#' Plots the probability values as a barplot.
#'
#' @return No return value, this function generates a plot in the current graphics device.
#'
#' @export
#'
#' @examples
#' x <- seq(0, 5)
#' y <- dpois(x, lambda = 2)
#' plot_pmf(x, y)
plot_pmf <- function(x, y, ..., col = "grey", add = FALSE) {
  if (isFALSE(add)) {
    pars_list <- list(
      xlim = range(x) + c(-0.5, 0.5),
      ylim = c(0, max(y)),
      type = "n",
      xaxt = "n",
      xlab = "",
      ylab = "",
      x = x,
      y = y
    )

    for (i in ...names()) pars_list[[i]] <- list(...)[[i]]

    do.call(plot, pars_list)
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
#' @param collapse logical. Collapse result of glue() with [`glue_collapse()`][glue::glue_collapse()]?
#' @param collapse_sep string. A character string to separate the original strings in the collapsed string.
#'
#' @return A character (of class `glue`).
#' @examples
#' name <- "Tom"
#' psborrow2:::h_glue("hello, my name is {{name}}.")
#' name <- c("Tom", "Fred")
#' psborrow2:::h_glue("hello, my name is {{name}}.", collapse = TRUE)
#' @noRd
h_glue <- function(..., collapse = FALSE, collapse_sep = "\n") {
  result <- glue::glue(..., .open = "{{", .close = "}}", .envir = parent.frame())
  if (isTRUE(collapse)) {
    result <- glue::glue_collapse(result, sep = collapse_sep)
  }
  result
}


#' Get constraints from a list of `Prior`s
#'
#' @param cov_obj A `Covariates` object.
#'
#' @return A `matrix` with columns "lower" and "upper" with rows for each `Prior`.
#' @examples
#' get_covariate_constraints(
#'   add_covariates(
#'     c("cov1", "cov2", "cov3"),
#'     list(
#'       prior_normal(0, 10),
#'       prior_beta(0.3, 0.3),
#'       prior_gamma(30, 1)
#'     )
#'   )
#' )
#' @noRd
get_covariate_constraints <- function(cov_obj) {
  n_covs <- length(cov_obj@covariates)
  if (is(cov_obj@priors, "Prior")) {
    cons <- rep(parse_constraint(cov_obj@priors), each = n_covs)
    cons <- matrix(cons, ncol = 2, dimnames = list(NULL, c("lower", "upper")))
  } else {
    cons <- t(vapply(cov_obj@priors, function(p) parse_constraint(p), numeric(2L)))
  }
  assert_numeric(cons, any.missing = FALSE, len = 2 * n_covs)
  cons
}

#' Extract Upper and Lower Bounds from a Prior object
#'
#' @param object `Prior` Object of class Prior
#'
#' @return
#' A list with upper and lower bounds. Any unspecified bounds are set to `-Inf` or `Inf`.
#' @examples
#' np <- prior_normal(0, 100)
#' parse_constraint(np)
#' @noRd
parse_constraint <- function(object) {
  assert_class(object, "Prior")
  s <- eval_constraints(object)
  s <- gsub("[<>[:space:]]", "", s)
  s_list <- strsplit(s, ",")[[1]]

  lower <- as.numeric(
    gsub("lower[[:space:]]*=[[:space:]]*", "", s_list[grepl("lower", s_list)])
  )

  upper <- as.numeric(
    gsub("upper[[:space:]]*=[[:space:]]*", "", s_list[grepl("upper", s_list)])
  )

  c(lower = max(lower, -Inf, na.rm = TRUE), upper = min(upper, Inf, na.rm = TRUE))
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
#' if (check_cmdstan()) {
#'   analysis_object <- create_analysis_obj(
#'     data_matrix = example_matrix,
#'     covariates = add_covariates(
#'       covariates = c("cov1", "cov2"),
#'       priors = prior_normal(0, 1000)
#'     ),
#'     outcome = outcome_surv_exponential(
#'       "time",
#'       "cnsr",
#'       baseline_prior = prior_normal(0, 1000)
#'     ),
#'     borrowing = borrowing_hierarchical_commensurate(
#'       "ext",
#'       prior_exponential(.001)
#'     ),
#'     treatment = treatment_details(
#'       "trt",
#'       prior_normal(0, 1000)
#'     )
#'   )
#'   samples <- mcmc_sample(analysis_object, 200, 400, 1)
#'   draws <- samples$draws()
#'   renamed_draws <- rename_draws_covariates(draws, analysis_object)
#'   summary(renamed_draws)
#' }
rename_draws_covariates <- function(draws, analysis) {
  assert_class(draws, "draws")
  assert_class(analysis, "Analysis")
  names <- variable_dictionary(analysis)
  names_list <- setNames(as.list(names$Stan_variable), names$Description)
  do.call(posterior::rename_variables, args = c(list(.x = draws), names_list))
}


#' Create Variable Dictionary
#'
#' @param analysis_obj `Analysis`. Object to describe variable names.
#'
#' @return A `data.frame` with the names of Stan variables and the descriptions.
#' @export
variable_dictionary <- function(analysis_obj) {
  assert_class(analysis_obj, "Analysis")

  beta_covariates <- if (!is.null(analysis_obj@covariates)) analysis_obj@covariates@name_betas else NULL
  beta_trt <- analysis_obj@outcome@name_beta_trt
  exp_trt <- analysis_obj@outcome@name_exp_trt
  alpha_type <- analysis_obj@outcome@alpha_type
  addnl_params <- analysis_obj@outcome@name_addnl_params
  tau <- create_tau_string(analysis_obj@borrowing)
  alpha <- create_alpha_string(analysis_obj@borrowing, analysis_obj@outcome)

  vars <- c(tau, alpha, beta_covariates, beta_trt, exp_trt, addnl_params)
  data.frame(Stan_variable = unname(vars), Description = names(vars))
}

#' Get Stan code for a `Prior`
#'
#' @param object `Prior` or list of `Prior` objects.
#'
#' @return A string containing the Stan code sampling from specified distribution.
#' @examples
#' get_prior_string(prior_normal(0, 100))
#' @noRd
get_prior_string <- function(object) {
  assert_multi_class(object, c("Prior", "list"))
  if (is(object, "list")) {
    assert_list(object, types = "Prior")
    vapply(
      object,
      function(object) h_glue(object@stan_code),
      character(1L)
    )
  } else {
    h_glue(object@stan_code)
  }
}
