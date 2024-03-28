#' `Prior` Class
#'
#' A class for defining priors to be translated to Stan code. Objects of class
#' `Prior` should not be created directly but by one of the specific prior
#' class constructors.
#'
#' @slot stan_code character. Stan implementation of the prior, with
#' placeholders for parameters surrounded with `{{` and `}}` to be replaced
#' with [glue::glue()].
#' @slot n_param integer. Number of prior parameters.
#' @slot constraint character. Support of prior distribution expressed as a
#' Stan constraint, e.g. `"<lower=0, upper=1>"`.
#' @family prior classes
#' @seealso Prior constructor functions: [prior_bernoulli()], [prior_beta()], [prior_cauchy()], [prior_half_cauchy()],
#' [prior_gamma()], [prior_normal()], [prior_poisson()], [uniform_prior()]
setClass(
  "Prior",
  slots = c(
    stan_code = "character",
    n_param = "integer",
    constraint = "character"
  ),
  validity = function(object) {
    check_string(object@stan_code)
    check_integer(object@n_param, lower = 1, len = 1, any.missing = FALSE)
    check_string(object@constraint)
  },
  contains = "VIRTUAL"
)

# show ----
setMethod(
  f = "show",
  signature = "Prior",
  definition = function(object) {
    cat(
      class(object)[1],
      "object with parameters",
      h_glue(object@stan_code)
    )
  }
)

# plot ----

#' @rdname plot
#' @param default_limits Numeric range to plot distribution over.
#' @param dist_type Plot a continuous or discrete distribution.
#' @param density_fun Function which takes a vector of values and returns a vector of density values.
setMethod(
  f = "plot",
  signature = c("Prior", "missing"),
  definition = function(x, y, default_limits, dist_type = c("continuous", "discrete"), density_fun, add, ...) {
    assert_numeric(default_limits, finite = TRUE, any.missing = FALSE, len = 2)
    dist_type <- match.arg(dist_type)
    assert_function(density_fun)
    assert_flag(add)

    limits <- if (!is.na(xlim_arg_n <- match("xlim", ...names()))) {
      ...elt(xlim_arg_n)
    } else if (isTRUE(add)) {
      par("usr")[1L:2L]
    } else {
      default_limits
    }

    values <- if (dist_type == "discrete") {
      seq(from = ceiling(limits[1]), to = floor(limits[2]))
    } else {
      n <- if (!is.na(n_arg_n <- match("n", ...names()))) ...elt(n_arg_n) else 301
      seq(from = limits[1], to = limits[2], length = n)
    }

    y <- density_fun(values)

    if (dist_type == "continuous") {
      if (isTRUE(add)) lines(values, y, ...) else plot_pdf(values, y, ...)
    } else {
      plot_pmf(values, y, add = add, ...)
    }
  }
)

# evaluate constraints----

#' Evaluate constraints
#'
#' Evaluate constraints when these are called
#'
#' @param object `Prior` object
#'
#' @rdname eval_constraints
#'
setGeneric("eval_constraints", function(object) standardGeneric("eval_constraints"))

#' @rdname eval_constraints
setMethod(
  f = "eval_constraints",
  signature = c("Prior"),
  definition = function(object) {
    return(h_glue(object@constraint))
  }
)
