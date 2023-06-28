

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
#' @return `BaselineObject` to build simulated dataset
#' @export
#'
#' @examples
#' corr_covs <- add_covariates(
#'   names = c("b1", "b2"),
#'   means_int = c(5, 25),
#'   covariance_int = covariance_matrix(diag = c(1, 1), upper_tri = 0.4)
#' )
add_covariates <- function(names, means_int, means_ext = means_int, covariance_int, covariance_ext = covariance_int) {
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


# BaselineObject -----

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

#' Create Baseline Data Simulation Object
#'
#' @param n_trt_int Number of internal treated patients
#' @param n_ctrl_int Number of internal control patients
#' @param n_ctrl_ext Number of external control patients
#' @param covariates List of correlated covariates objects, see [add_covariates()]
#' @param transformations List of named transformation functions.
#'
#' @details
#' Transformation functions are evaluated in order and create or overwrite a column
#' in the data.frame with that name. The function should take a [BaselineDataFrame]
#' object and return a vector with length identical to the total number of patients.
#' The `@BaselineObject` slot may be accessed directly or with [get_quantiles()] to
#' create transformations. See [binary_cutoff()]
#'
#' @return A `BaselineObject`
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
#'   covariates = add_covariates(
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
  assert_integerish(n_trt_int, len = 1, lower = 1)
  assert_integerish(n_ctrl_int, len = 1, lower = 1)
  assert_integerish(n_ctrl_ext, len = 1, lower = 1)

  if (!missing(covariates)) {
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


#' @importFrom generics generate
#' @importFrom mvtnorm rmvnorm
generate.BaselineObject <- function(x, ...) {
  arm_data <- data.frame(
    patid = seq_len(x@n_trt_int + x@n_ctrl_int + x@n_ctrl_ext),
    ext = rep(c(0, 1), times = c(x@n_trt_int + x@n_ctrl_int, x@n_ctrl_ext)),
    trt = rep(c(1, 0), times = c(x@n_trt_int, x@n_ctrl_int + x@n_ctrl_ext))
  )

  # If any covariates are defined, generate multivariate normal data and combine with arm data
  cov_defined <- vapply(x@covariates, function(x) length(x@names) > 0, logical(1L))
  if (any(cov_defined)) {
    cor_data_list <- lapply(
      x@covariates[cov_defined],
      function(cor_cov, n_int = x@n_trt_int + x@n_ctrl_int, n_ext = x@n_ctrl_ext) {
        mvnorm_data <- rbind(
          mvtnorm::rmvnorm(n = n_int, mean = cor_cov@means_int, sigma = cor_cov@covariance_int),
          mvtnorm::rmvnorm(n = n_ext, mean = cor_cov@means_ext, sigma = cor_cov@covariance_ext)
        )
        colnames(mvnorm_data) <- cor_cov@names
        as.data.frame(mvnorm_data)
      }
    )
    arm_data <- cbind(arm_data, do.call("cbind", cor_data_list))
  }

  bl_df <- .baseline_dataframe(arm_data, BaselineObject = x)

  # For each named transformation, either create a new column or overwrite
  for (i in names(x@transformations)) {
    var_index <- match(i, bl_df@names)
    if (!is.na(var_index)) {
      bl_df@.Data[[var_index]] <- x@transformations[[i]](bl_df)
    } else {
      next_index <- length(bl_df@names) + 1
      bl_df@.Data[[next_index]] <- x@transformations[[i]](bl_df)
      bl_df@names[next_index] <- i
    }
  }
  bl_df
}


setMethod(
  f = "generate",
  signature = "BaselineObject",
  definition = generate.BaselineObject
)


#' Baseline Data Frame Object
#'
#' A `data.frame` with additional slot containing simulation definition
#'
#' @slot BaselineObject Simulated covariates definitions as `BaselineObject`. See [create_baseline_object()]
#'
#' @return A `BaselineDataFrame`
.baseline_dataframe <- setClass(
  "BaselineDataFrame",
  contains = "data.frame",
  slots = c(
    BaselineObject = "BaselineObject"
  )
)

# Validity checks for Baseline Object
setValidity("BaselineObject", function(object) {
  msg <- NULL
  c(msg, check_list(object@covariates, types = "CorrelatedCovariates"))
  c(msg, check_list(object@transformations, types = "function", names = "named"))
  if (is.null(msg)) TRUE else msg
})


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
#'
get_quantiles <- function(object, var) {
  assert_class(object, "BaselineDataFrame")

  covs <- object@BaselineObject@covariates
  names <- unlist(lapply(covs, function(x) x@names))
  assert_subset(var, choices = names)

  index <- which(names == var)

  mean_int <- unlist(lapply(covs, function(x) x@means_int))[index]
  mean_ext <- unlist(lapply(covs, function(x) x@means_ext))[index]

  sd_int <- unlist(lapply(covs, function(x) sqrt(diag(x@covariance_int))))[index]
  sd_ext <- unlist(lapply(covs, function(x) sqrt(diag(x@covariance_ext))))[index]

  ifelse(
    object[["ext"]] == 0,
    pnorm(object[[var]], mean_int, sd_int),
    pnorm(object[[var]], mean_ext, sd_ext)
  )
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
    ifelse(ext, q > int_cutoff, q > ext_cutoff)
  }
}
