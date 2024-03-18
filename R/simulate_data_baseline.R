#' Create Covariance Matrix
#'
#' @param diag Diagonal entries of the covariance matrix
#' @param upper_tri Upper triangle entries of the matrix, specified column wise.
#'
#' @return A symmetric matrix with `diag` values on the main diagonal and
#' `upper_tri` values in the lower and upper triangles.
#' @export
#'
#' @examples
#' m1 <- covariance_matrix(c(1, 1, 1, 1), c(.8, .3, .8, 0, 0, 0))
#' m1
#' mvtnorm::rmvnorm(5, mean = c(0, 0, 0, 0), sigma = m1)
#'
#' # No correlation
#' covariance_matrix(c(1, 2, 3))
#'
#' @importFrom Matrix nearPD
covariance_matrix <- function(diag, upper_tri) {
  assert_numeric(diag, lower = 0)
  dim <- length(diag)
  if (missing(upper_tri)) upper_tri <- rep(0, sum(seq.int(1, dim - 1)))

  if (dim == 1) {
    cov_mat <- matrix(diag)
  } else if (dim > 1) {
    assert_numeric(upper_tri, len = sum(seq.int(1, dim - 1)))
    cov_mat <- diag(diag, nrow = dim, ncol = dim)
    cov_mat[upper.tri(cov_mat)] <- upper_tri
    cov_mat[lower.tri(cov_mat)] <- t(cov_mat)[lower.tri(cov_mat)]
  }
  if (any(eigen(cov_mat, only.values = TRUE)$values < 0)) {
    warning(
      "Provided parameters do not define a semi-positive definite matrix. ",
      "Finding nearest positive definite matrix with Matrix::nearPD()."
    )
    cov_mat <- Matrix::nearPD(cov_mat, ensureSymmetry = TRUE, keepDiag = TRUE, base.matrix = TRUE)$mat
  }
  cov_mat
}


#' Specify Correlated Baseline Covariates
#'
#' Set parameters to generate correlated multivariate normal data for internal and external patients.
#'
#' @param names character vector of variable names.
#' @param means_int numeric vector of means for internal patients. Must have same length as `names`
#' @param means_ext numeric vector of means for external patients. Must have same length as `names`
#' @param covariance_int variance-covariance matrix for generating multivariate normal for internal patients.
#' Must be square matrix with same number of rows and `length(names)`
#' @param covariance_ext variance-covariance matrix for generating multivariate normal data for external patients.
#' Must be square matrix with same number of rows and `length(names)`
#'
#' @return [BaselineObject][BaselineObject-class] to build simulated dataset
#' @export
#'
#' @examples
#' corr_covs <- baseline_covariates(
#'   names = c("b1", "b2"),
#'   means_int = c(5, 25),
#'   covariance_int = covariance_matrix(diag = c(1, 1), upper_tri = 0.4)
#' )
baseline_covariates <- function(names,
                                means_int,
                                means_ext = means_int,
                                covariance_int,
                                covariance_ext = covariance_int) {
  assert_character(names)
  n <- length(names)
  assert_numeric(means_int, finite = TRUE, len = n, any.missing = FALSE)
  assert_numeric(means_ext, finite = TRUE, len = n, any.missing = FALSE)
  assert_matrix(covariance_int, nrows = n, ncols = n, any.missing = FALSE)
  assert_matrix(covariance_ext, nrows = n, ncols = n, any.missing = FALSE)

  .correlated_covariates(
    names = names,
    means_int = means_int,
    means_ext = means_ext,
    covariance_int = covariance_int,
    covariance_ext = covariance_ext
  )
}


.correlated_covariates <- setClass(
  "CorrelatedCovariates",
  slots = c(
    names = "character",
    means_int = "numeric",
    means_ext = "numeric",
    covariance_int = "matrix",
    covariance_ext = "matrix"
  )
)

setMethod(
  "show",
  signature = c(object = "CorrelatedCovariates"),
  function(object) {
    print(
      data.frame(
        covariate = object@names,
        means_internal = object@means_int,
        means_external = object@means_ext
      ),
      row.names = FALSE
    )
    cat("\n")
    m1 <- object@covariance_int
    m2 <- object@covariance_ext
    dimnames(m1) <- dimnames(m2) <- list(object@names, object@names)
    outputs <- utils::capture.output({
      print(m1)
      print(m2)
    })
    width <- nchar(outputs[1])
    half <- length(outputs) / 2
    cat("Covariance Matrices\n")
    if (options("width") < width * 2 + 1) {
      cat("Internal\n")
      for (i in seq_len(half)) cat(outputs[i], "\n")
      cat("\nExternal\n")
      for (i in seq_len(half)) cat(outputs[i + half], "\n")
    } else {
      m_gap <- strrep(" ", 1 + max(0, 8 - width))
      h_gap <- strrep(" ", 1 + max(0, width - 8))
      cat("Internal", h_gap, "External\n")
      for (i in seq_len(half)) cat(outputs[i], m_gap, outputs[i + half], "\n")
    }
  }
)

# BaselineObject -----

#' `BaselineObject` class for data simulation
#'
#' @slot n_trt_int integer. Number of internal treated patients
#' @slot n_ctrl_int integer. Number of internal control patients
#' @slot n_ctrl_ext integer. Number of external control patients
#' @slot covariates list. List of correlated covariates objects, see [baseline_covariates()]
#' @slot transformations list. List of named transformation functions.
#'
.baseline_object <- setClass(
  "BaselineObject",
  slots = c(
    n_trt_int = "integer",
    n_ctrl_int = "integer",
    n_ctrl_ext = "integer",
    covariates = "list",
    transformations = "list"
  )
)

# Validity checks for Baseline Object
setValidity("BaselineObject", function(object) {
  msg <- NULL
  c(msg, check_list(object@covariates, types = "CorrelatedCovariates"))
  c(msg, check_list(object@transformations, types = "function", names = "named"))
  if (is.null(msg)) TRUE else msg
})

#' Create Baseline Data Simulation Object
#'
#' @param n_trt_int Number of internal treated patients
#' @param n_ctrl_int Number of internal control patients
#' @param n_ctrl_ext Number of external control patients
#' @param covariates List of correlated covariates objects, see [baseline_covariates()]
#' @param transformations List of named transformation functions.
#'
#' @details
#' Transformation functions are evaluated in order and create or overwrite a column
#' in the data.frame with that name. The function should take a `data.frame` (specifically
#' a `BaselineDataFrame` object from `generate(BaselineObject)`) and return a vector with
#' length identical to the total number of patients.
#' The `@BaselineObject` slot may be accessed directly or with [get_quantiles()] to
#' create transformations. See [binary_cutoff()]
#'
#' @return A [BaselineObject][BaselineObject-class]
#' @export
#'
#' @examples
#' bl_no_covs <- create_baseline_object(
#'   n_trt_int = 100,
#'   n_ctrl_int = 50,
#'   n_ctrl_ext = 100
#' )
#'
#'
#' bl_biomarkers <- create_baseline_object(
#'   n_trt_int = 100,
#'   n_ctrl_int = 50,
#'   n_ctrl_ext = 100,
#'   covariates = baseline_covariates(
#'     c("b1", "b2", "b3"),
#'     means_int = c(0, 0, 0),
#'     covariance_int = covariance_matrix(c(1, 1, 1), c(.8, .3, .8))
#'   ),
#'   transformations = list(
#'     exp_b1 = function(data) exp(data$b1),
#'     b2 = binary_cutoff("b2", int_cutoff = 0.7, ext_cutoff = 0.5)
#'   )
#' )
#'
create_baseline_object <- function(n_trt_int, n_ctrl_int, n_ctrl_ext, covariates, transformations) {
  assert_integerish(n_trt_int, len = 1, lower = 0)
  assert_integerish(n_ctrl_int, len = 1, lower = 0)
  assert_integerish(n_ctrl_ext, len = 1, lower = 0)

  if (!missing(covariates)) {
    if (is(covariates, "CorrelatedCovariates")) covariates <- list(covariates)
    assert_list(covariates)
  } else {
    covariates <- list(.correlated_covariates())
  }

  if (!missing(transformations)) {
    assert_list(transformations)
  } else {
    transformations <- list()
  }

  .baseline_object(
    n_trt_int = as.integer(n_trt_int),
    n_ctrl_int = as.integer(n_ctrl_int),
    n_ctrl_ext = as.integer(n_ctrl_ext),
    covariates = covariates,
    transformations = transformations
  )
}

setMethod(
  "show",
  signature = c(object = "BaselineObject"),
  function(object) {
    cat("Baseline Data Simulation Object\n")
    cat("  N internal treated: ", object@n_trt_int, "\n")
    cat("  N internal control: ", object@n_ctrl_int, "\n")
    cat("  N external control: ", object@n_ctrl_ext, "\n")
    cat("\n")
    if (length(object@covariates)) {
      cat("Covariates: \n")
      print(object@covariates)
    }
    if (length(object@transformations)) {
      cat("Transformations: \n")
      cat("  ", toString(names(object@transformations)), "\n")
    }
  }
)

#' Set Transformations in Baseline Objects
#'
#' @param object `BaselineObject` created by [create_baseline_object].
#' @param ... named transformation functions. See details for more information.
#' @param overwrite If `TRUE` overwrite existing transformation list and only include newly specified transformations.
#'
#' @details Transformation functions are evaluated in order and create or overwrite a column in the data.frame with that
#'   name. The function should have signature `function(data)`, taking a `data.frame` (specifically a
#'   `BaselineDataFrame` object from `generate(BaselineObject)`) and return a vector with length identical to the total
#'   number of patients. The `@BaselineObject` slot of the [BaselineDataFrame-class] may be accessed directly or with
#'   [get_quantiles()] to create transformations. See [binary_cutoff()].
#' @return An updated `BaselineObject`
#' @export
#'
#' @examples
#' baseline <- create_baseline_object(
#'   100, 50, 100,
#'   covariates = baseline_covariates(
#'     names = "age", means_int = 55,
#'     covariance_int = covariance_matrix(5)
#'   )
#' )
#' set_transformations(baseline, age_scaled = function(data) scale(data$age))
setMethod(
  f = "set_transformations",
  signature = "BaselineObject",
  definition = function(object, ..., overwrite = FALSE) {
    assert_class(object, "BaselineObject")
    new_tfs <- list(...)
    assert_list(new_tfs, types = "function", names = "ids", .var.name = "Transformations in ...")
    assert_flag(overwrite)
    updated_tfs <- if (overwrite) new_tfs else c(object@transformations, new_tfs)
    tfs_names <- names(updated_tfs)
    tfs_duplicates <- tfs_names[anyDuplicated(tfs_names)]
    if (length(tfs_duplicates)) {
      warning(
        "Multiple transformation functions named: ", toString(tfs_duplicates), ". ",
        "These will be applied sequentially.\n"
      )
    }
    object@transformations <- updated_tfs
    validObject(object)
    object
  }
)

#' @importFrom generics generate
#' @importFrom mvtnorm rmvnorm
# nolint start
generate.BaselineObject <- function(x, ...) {
  # nolint end
  arm_data <- .mapply(
    function(n, id_offset, ext, trt) {
      .baseline_dataframe(data.frame(patid = id_offset + seq_len(n), ext = rep(ext, n), trt = rep(trt, n)))
    },
    dots = list(
      n = list(x@n_trt_int, x@n_ctrl_int, x@n_ctrl_ext),
      id_offset = list(0, x@n_trt_int, x@n_trt_int + x@n_ctrl_int),
      ext = list(0, 0, 1),
      trt = list(1, 0, 0)
    ),
    MoreArgs = NULL
  )

  # If any covariates are defined, generate multivariate normal data and combine with arm data
  cov_defined <- vapply(x@covariates, function(x) length(x@names) > 0, logical(1L))
  if (any(cov_defined)) {
    for (cov in x@covariates[cov_defined]) {
      arm_data <- .mapply(
        dots = list(
          data = arm_data,
          mean = list(cov@means_int, cov@means_int, cov@means_ext),
          sigma = list(cov@covariance_int, cov@covariance_int, cov@covariance_int)
        ),
        MoreArgs = list(names = cov@names),
        FUN = function(data, mean, sigma, names) {
          covs <- if (nrow(data)) {
            mvtnorm::rmvnorm(n = nrow(data), mean = mean, sigma = sigma)
          } else {
            matrix(ncol = length(names), nrow = 0)
          }
          covs <- setNames(data.frame(covs), names)
          cbind2(data, .baseline_dataframe(covs, cov_names = names, means = mean, variances = diag(sigma)))
        }
      )
    }
  }

  # Apply transformations
  if (length(x@transformations)) {
    arm_data <- lapply(arm_data, function(data) {
      for (i in names(x@transformations)) {
        data[i] <- x@transformations[[i]](data)
      }
      data
    })
  }
  .baseline_data_list(arm_data, baseline_object = x)
}


#' Generate Data for a `BaselineObject`
#'
#' @param x a `BaselineObject` object created by [create_baseline_object]
#' @param ... additional parameters are ignored
#'
#' @return A [BaselineDataFrame][psborrow2::BaselineDataFrame-class] object
#' @export
#'
#' @examples
#' bl_biomarkers <- create_baseline_object(
#'   n_trt_int = 100,
#'   n_ctrl_int = 50,
#'   n_ctrl_ext = 100,
#'   covariates = baseline_covariates(
#'     c("b1", "b2", "b3"),
#'     means_int = c(0, 0, 0),
#'     covariance_int = covariance_matrix(c(1, 1, 1), c(.8, .3, .8))
#'   ),
#'   transformations = list(
#'     exp_b1 = function(data) exp(data$b1),
#'     b2 = binary_cutoff("b2", int_cutoff = 0.7, ext_cutoff = 0.5)
#'   )
#' )
#' generate(bl_biomarkers)
setMethod(
  f = "generate",
  signature = "BaselineObject",
  definition = generate.BaselineObject
)

# BaselineDataFrame Object -----------

#' Baseline Data Frame Object
#'
#' Contains a generated baseline dataset for a single arm.
#' @slot cov_names `character` contains the names of covariates generated from the multivariate normal distribution
#' @slot means `numeric` contains the means of generating distribution for the covariates in `cov_names`
#' @slot variances `numeric` contains the marginal variances of generating distribution for the covariates in
#' `cov_names`.
#'
#' @return A `BaselineDataFrame`
.baseline_dataframe <- setClass(
  "BaselineDataFrame",
  contains = "data.frame",
  slots = c(
    cov_names = "character",
    means = "numeric",
    variances = "numeric"
  )
)

# Validity checks for Baseline Object
setValidity("BaselineDataFrame", function(object) {
  msg <- NULL
  c(msg, check_names(object@cov_names, subset.of = object@names))
  c(msg, check_numeric(object@means, finite = TRUE, any.missing = FALSE, len = length(object@cov_names)))
  c(msg, check_numeric(object@variances, lower = 0, finite = TRUE, any.missing = FALSE, len = length(object@cov_names)))
  if (is.null(msg)) TRUE else msg
})

# cbind BaselineDataFrame
#' @export
cbind.BaselineDataFrame <- function(...) {
  to_bind <- list(...)
  assert_list(to_bind, types = "BaselineDataFrame", len = 2)
  .baseline_dataframe(
    data.frame(to_bind[[1]], to_bind[[2]]),
    cov_names = c(to_bind[[1]]@cov_names, to_bind[[2]]@cov_names),
    means = c(to_bind[[1]]@means, to_bind[[2]]@means),
    variances = c(to_bind[[1]]@variances, to_bind[[2]]@variances)
  )
}

setMethod(
  f = "cbind2",
  signature(x = "BaselineDataFrame", y = "BaselineDataFrame"),
  definition = function(x, y) cbind.BaselineDataFrame(x, y)
)


# show method
setMethod(
  f = "show",
  signature = "BaselineDataFrame",
  definition = function(object) {
    cat("Simulated Baseline Data")
    cat("\n")
    print.data.frame(object)
  }
)

#' Get Quantiles of Random Data
#'
#' Helper for use within transformation functions for [create_baseline_object()].
#'
#' @param object a `BaselineDataFrame`
#' @param var character string name of the variable
#'
#' @return A numeric vector containing quantiles based on the data
#'  generating distribution.
#' @export
#' @importFrom stats pnorm
#'
get_quantiles <- function(object, var) {
  assert_class(object, "BaselineDataFrame")
  assert_choice(var, object@cov_names)
  i <- match(var, object@cov_names)
  pnorm(object[var][[1]], object@means[i], sqrt(object@variances[i]))
}


#' Binary Cut-Off Transformation
#'
#' @param name variable to transform
#' @param int_cutoff cut-off for internal patients, numeric between 0 and 1
#' @param ext_cutoff cut-off for external patients, numeric between 0 and 1
#'
#' @return Transformation function to be used in [create_baseline_object()].
#'  Sets quantile values larger than cut-off value to `TRUE` otherwise `FALSE`.
#' @export
#' @examples
#' # Creates a simple function, where `data` is a `BaselineDataFrame`:
#' function(data) {
#'   ext <- data$ext == 0
#'   q <- get_quantiles(data, name)
#'   ifelse(ext, q > int_cutoff, q > ext_cutoff)
#' }
#'
binary_cutoff <- function(name, int_cutoff, ext_cutoff) {
  assert_character(name, len = 1, any.missing = FALSE)
  assert_numeric(int_cutoff, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  assert_numeric(ext_cutoff, lower = 0, upper = 1, any.missing = FALSE, len = 1)
  function(data) {
    ext <- data$ext == 0
    q <- get_quantiles(data, name)
    as.integer(ifelse(ext, q > int_cutoff, q > ext_cutoff))
  }
}

# get_vars ---
#' @rdname get_vars
#' @include generics.R
setMethod(
  f = "get_vars",
  signature = "BaselineObject",
  definition = function(object) {
    unique(c(
      unlist(lapply(object@covariates, slot, name = "names")),
      names(object@transformations)
    ))
  }
)


#' Get All Variable Names in Simulated Data Model Matrix
#'
#' @param object `BaselineObject`
#'
#' @return A vector of variable names
possible_data_sim_vars <- function(object) {
  object@n_trt_int <- 1L
  object@n_ctrl_int <- 0L
  object@n_ctrl_ext <- 0L
  df <- generate(object)[[1]]
  formula_names <- setdiff(colnames(df), c("patid", "ext", "trt"))
  formula <- as.formula(paste(c("~ 0", formula_names), collapse = " + "))
  mm <- model.matrix(formula, data = data.frame(df))
  colnames(mm)
}


#' Baseline Data Frame List
#'
#' A named `list` of `BaselineDataFrame`s with generated data for `internal_treated`/`internal_control`/
#' `external_control` groups
#' @slot baseline_object Simulated covariates definitions as `BaselineObject`. See [create_baseline_object()]
#'
#' @return A `BaselineDataList`
.baseline_data_list <- setClass(
  "BaselineDataList",
  contains = "list",
  slots = c(baseline_object = "BaselineObject")
)

#' @rdname as_data_frame
#' @export
# nolint start
as.data.frame.BaselineDataList <- function(x, ...) {
  # nolint end
  df <- do.call(rbind, x)
  data.frame(setNames(df@.Data, df@names), ...)
}

setAs(
  from = "BaselineDataList",
  to = "data.frame",
  def = function(from) as.data.frame.BaselineDataList(from)
)

#' @importFrom utils head
setMethod(
  "show",
  signature = c(object = "BaselineDataList"),
  function(object) {
    cat("Baseline Data List\n\n")
    cat("Internal Treated\n")
    print(head(object[[1]]), row.names = FALSE)
    cat("Internal Control\n")
    print(head(object[[2]]), row.names = FALSE)
    cat("External Control\n")
    print(head(object[[3]]), row.names = FALSE)
  }
)
