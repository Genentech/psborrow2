#' `SimDataList` Class
#'
#' A class for defining generated data for use as part of a
#' simulation study. Objects of class `SimDataList` should not be created
#' directly but by the constructor `sim_data_list()`.
#'
#' @slot data_list list of lists of matrices. The lists at the highest
#' level differ in that the parameters used to generate the data. The matrices
#' at lowest level are different iterations of the same data generation
#' parameters.
#' @slot guide data.frame. `guide` contains information on the parameters
#' that differ at the highest level of `data_list`.
#' @slot effect character. The column in `guide` that
#' corresponds to the true treatment effect estimate (hazard ratio or odds ratio).
#' @slot drift character. The column in `guide` that
#' corresponds to the drift between external and internal control arms. A
#' drift >1 means the external arm experiences greater effects.
#' @slot index character. The column in `guide` that corresponds
#' to the index of the parameter situations in `data_list`.
.sim_data_list <- setClass(
  "SimDataList",
  slots = c(
    data_list = "list",
    guide = "data.frame",
    effect = "character",
    drift = "character",
    index = "character"
  ),
  validity = function(object) {
    # Format is list of lists
    if (!all(vapply(object@data_list, function(item) is(item, "list"), FUN.VALUE = logical(1)))) {
      return("`data_list` must be a list of lists")
    }

    # At lowest level there are matrices
    if (!all(vapply(object@data_list, function(item) {
      all(vapply(item, function(mat) {
        is(mat, "matrix")
      }, FUN.VALUE = logical(1)))
    }, FUN.VALUE = logical(1)))) {
      return("`data_list` lowest items must be matrices")
    }

    # Guide and data are same length
    if (NROW(object@guide) != NROW(object@data_list)) {
      return("`guide` and `data_list` must be same length")
    }

    # Effect, drift, and index are columns in guide
    if (!object@effect %in% colnames(object@guide)) {
      return("`effect` must be a column in `guide`")
    }

    if (!object@drift %in% colnames(object@guide)) {
      return("`drift` must be a column in `guide`")
    }

    if (!object@index %in% colnames(object@guide)) {
      return("`index` must be a column in `guide`")
    }

    # n_datasets_per_list is protected
    if ("n_datasets_per_param" %in% colnames(object@guide)) {
      return("'n_datasets_per_param' is a protected column name in `guide`.")
    }

    # index items must be unique
    if (length(unique(object@guide[[object@index]])) != NROW(object@data_list)) {
      return("`index` column in guide must be unique, one for each entry in data_list")
    }

    # index must be coercable to integer
    if (!test_integerish(object@guide[[object@index]])) {
      return("`index` column in guide must of type integer")
    }
  }
)

#' Input generated data for a simulation study
#'
#' A function for defining generated data for use as part of a
#' simulation study.
#'
#' @param data_list list of lists of matrices. The lists at the highest
#' level differ in that the parameters used to generate the data. The matrices
#' at lowest level are different iterations of the same data generation
#' parameters. See `details`.
#' @param guide data.frame. `guide` contains information on the parameters
#' that differ at the highest level of `data_list`. See `details.`
#' @param effect character. The column in `guide` that
#' corresponds to the true treatment effect estimate (hazard ratio or odds ratio).
#' @param drift character. The column in `guide` that
#' corresponds to the true drift effect estimate (hazard ratio or odds ratio).
#' A drift >1 means the external arm experiences greater effects.
#' @param index character. The column in `guide` that corresponds
#' to the index column.
#'
#' @details
#'
#' In this function, you are providing generated data for analysis in a
#' simulation study in `psborrow2`. Note that this function does not
#' do any data generation on your behalf; it assumes that you have generated
#' the data already. For a full working example, refer to the relevant vignette:
#' `vignette('simulation_study', package = 'psborrow2')`.
#'
#' More information on the inputs is provided below.
#'
#' ## Matrix requirements in `data_list`
#'
#' Each matrix embedded in `data_list` must have:
#' 1) a flag for whether the patient is an external control
#' 2) a flag for whether the patient is in the experimental treatment arm
#' 3) outcome information (time and censorship for survival, flag for
#' outcome in binary endpoints)
#'
#' Optionally, the matrices may also contain covariates. See `examples`.
#'
#' ## `data_list`
#'
#' Each set of distinct data generation parameters should be represented by
#' a single list of matrices. Because multiple scenarios may want to be
#' compared, a list of list of matrices is preferred. See `examples`.
#'
#' ## `guide`
#'
#' The `guide` should be a data.frame with one row per scenario. As a
#' consquence of this, the length of the list should equal the number of rows
#' in the guide. See `examples`.
#'
#' @family simulation classes
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
#'   ind = c(1, 2)
#' )
#'
#' sdl <- sim_data_list(
#'   data_list = data_list,
#'   guide = guide,
#'   effect = "trueOR",
#'   drift = "driftOR",
#'   index = "ind"
#' )
#' 
#' @return Object of class [`SimDataList`][SimDataList-class].
#' 
#' @export
sim_data_list <- function(data_list,
                          guide,
                          effect,
                          drift,
                          index) {
  sim_data_list <- .sim_data_list(
    data_list = data_list,
    guide = guide,
    effect = effect,
    drift = drift,
    index = index
  )

  sim_data_list@guide$n_datasets_per_param <- vapply(
    sim_data_list@data_list,
    NROW,
    FUN.VALUE = integer(1)
  )

  return(sim_data_list)
}

# show ----
setMethod(
  f = "show",
  signature = "SimDataList",
  definition = function(object) {
    cat("SimDataList object with ", NROW(object@data_list), " different scenarios\n")
    if (NROW(object@data_list) <= 10) {
      print(object@guide)
    }
  }
)

# c ----
#' @rdname c
#' 
#' @return list of [`SimDataList`][SimDataList-class] objects.
#' @export
setMethod(
  f = "c",
  signature = "SimDataList",
  definition = function(x, ...) {
    dots <- list(...)
    assert_list(dots, types = "SimDataList", .var.name = "...")

    x_cols <- colnames(x@guide)
    assertions <- makeAssertCollection()
    for (i in seq_along(dots)) {
      assert_names(
        colnames(dots[[i]]@guide),
        identical.to = x_cols,
        what = "colnames",
        .var.name = h_glue("Argument {{i+1}}"),
        add = assertions
      )
      assert_names(
        dots[[i]]@effect,
        identical.to = x@effect,
        .var.name = h_glue("Argument {{i+1}} @effect"),
        add = assertions
      )
      assert_names(
        dots[[i]]@drift,
        identical.to = x@drift,
        .var.name = h_glue("Argument {{i+1}} @drift"),
        add = assertions
      )
      assert_names(
        dots[[i]]@index,
        identical.to = x@index,
        .var.name = h_glue("Argument {{i+1}} @index"),
        add = assertions
      )
    }
    reportAssertions(assertions)

    combined_lists <- unlist(lapply(dots, slot, name = "data_list"), recursive = FALSE)
    new_data_list <- c(x@data_list, combined_lists)

    new_guides <- do.call(rbind, c(list(x@guide), lapply(dots, slot, name = "guide")))
    new_guides[x@index] <- seq_len(nrow(new_guides))
    new_guides$n_datasets_per_param <- NULL

    if (NROW(new_guides) != NROW(unique(new_guides[, x_cols[!x_cols %in% c("index", "n_datasets_per_param")]]))) {
      stop(
        "Duplicate scenarios detected.",
        "Please inspect the SimDataList objects you are combining to look for overlapping scenarios."
      )
    }

    sim_data_list(
      data_list = new_data_list,
      guide = new_guides,
      drift = x@drift,
      effect = x@effect,
      index = x@index
    )
  }
)

# get_data ----
#' @rdname get_data
setMethod("get_data", "SimDataList", function(object, index = NULL, dataset = NULL) {
  if (is.null(index) && is.null(dataset)) {
    return(object@data_list)
  }

  if (!is.null(index)) {
    assert_integerish(index, lower = 1, upper = length(object@data_list), any.missing = FALSE, null.ok = TRUE)
    if (is.null(dataset)) {
      return(object@data_list[[index]])
    } else {
      assert_integerish(dataset,
        lower = 1, upper = length(object@data_list[[index]]),
        any.missing = FALSE, null.ok = TRUE
      )
      return(object@data_list[[index]][[dataset]])
    }
  }
})
