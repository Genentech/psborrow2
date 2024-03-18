#' @include simulate_data_baseline.R

# Fixed External Control Data ---------------

#' @title Fixed External Control Data Object
#'
#' @slot data `data.frame` containing external control data
#' @slot n Number of observations
#'
#' @return A `FixedExternalData`
.datasim_fixed_external_data <- setClass(
  "DataSimFixedExternalData",
  slots = c(
    data = "data.frame",
    n = "integer"
  ),
  prototype = list(
    data = data.frame(),
    n = 0L
  )
)


#' Create a Fixed External Data Object
#'
#' @param data A `data.frame` containing external control data
#' @param req_cols A `character` vector of required covariate columns
#'
#' @return A `DataSimObject` with updated `enrollment_internal` and `enrollment_external` slots.
check_fixed_external_data <- function(data, req_cols) {
  assert_data_frame(data, min.rows = 1)
  cols <- colnames(data)

  if ("trt" %in% cols && !all(data[["trt"]] == 0)) {
    stop("All `trt` values must equal 0 in fixed external data")
  } else {
    data[["trt"]] <- 0
  }

  if ("ext" %in% cols && !all(data[["ext"]] == 1)) {
    stop("All `ext` values must equal 1 in fixed external data")
  } else {
    data[["ext"]] <- 1
  }

  missing_cols <- setdiff(req_cols, cols)
  if (length(missing_cols)) {
    stop("Columns required in data simulation are not in fixed external data: ", toString(missing_cols))
  }

  if (!"eventtime" %in% cols) stop("Fixed external data must contain column `eventtime`")
  if (!"status" %in% cols) stop("Fixed external data must contain column `status`")

  .datasim_fixed_external_data(data = data, n = nrow(data))
}


# Time to Event Distribution Object ------------------

#' Event Time Distribution Object
#'
#' @slot params Parameters used for simulating event times with [simsurv::simsurv()].
#' @slot label Description of the distribution.
.datasim_event <- setClass(
  "DataSimEvent",
  slots = c(
    params = "list",
    label = "character"
  ),
  prototype = list(
    params = list(),
    label = "No distribution specified"
  )
)

#' Specify a Time to Event Distribution
#'
#' Uses [simsurv::simsurv] to generate time to event data. See `simsurv` help for more details.
#'
#' @param dist Specify the distribution `"exponential"`
#' @param lambdas Scale parameter
#' @param gammas Second parameter needed for Weibull or Gompertz distributions
#' @param mixture Use mixture model?
#' @param pmix Proportion of mixtures
#' @param hazard A user defined hazard function
#' @param loghazard Alternatively, a user defined log hazard function
#' @param cumhazard Alternatively, a user defined cumulative hazard function
#' @param logcumhazard Alternatively, a user defined log cumulative hazard function
#' @param ... Other `simsurv` parameters
#'
#' @return A `SimDataEvent` object
#' @export
#'
#' @examples
#' weibull_surv <- create_event_dist(dist = "weibull", lambdas = 1 / 200, gammas = 1)
#' exp_event_dist <- create_event_dist(dist = "exponential", lambdas = 1 / 36)
create_event_dist <- function(dist = NULL,
                              lambdas = NULL,
                              gammas = NULL,
                              mixture = FALSE,
                              pmix = 0.5,
                              hazard = NULL,
                              loghazard = NULL,
                              cumhazard = NULL,
                              logcumhazard = NULL,
                              ...) {
  dist_spec <- c(
    dist = !is.null(dist), hazard = !is.null(hazard), loghazard = !is.null(loghazard),
    cumhazard = !is.null(cumhazard), logcumhazard = !is.null(logcumhazard)
  )
  dist_type <- names(dist_spec[dist_spec])
  if (length(dist_type) == 0) {
    stop("One of `dist`, `hazard`, `loghazard`, `cumhazard`, or `logcumhazard` must be specified")
  } else if (length(dist_type) > 1) {
    stop("Only one of `dist`, `hazard`, `loghazard`, `cumhazard`, or `logcumhazard` can be specified")
  }

  if (dist_type == "dist") {
    assert_choice(dist, choices = c("exponential", "weibull", "gompertz"))
  } else {
    assert_function(get(dist_type), args = c("t", "x", "betas"))
  }
  assert_numeric(lambdas, finite = TRUE, null.ok = TRUE, len = 1 + mixture)
  assert_numeric(gammas, finite = TRUE, null.ok = TRUE, len = 1 + mixture)
  assert_flag(mixture)
  assert_numeric(pmix, lower = 0, upper = 1, len = 1, any.missing = FALSE)

  label <- if (dist_type == "dist") {
    if (mixture) {
      h_glue("Mixture of {{dist}} distributions (p = {{pmix}}) ",
        "with lambdas = {{toString(lambdas)}}{{gamma_part}}",
        gamma_part = if (is.null(gammas)) "" else h_glue(" and gammas = {{toString(gammas)}}")
      )
    } else {
      h_glue("{{dist}} distribution with lambda = {{toString(lambdas)}}{{gamma_part}}",
        gamma_part = if (is.null(gammas)) "" else h_glue(" and gamma = {{toString(gammas)}}")
      )
    }
  } else {
    paste0("User defined ", dist_type)
  }

  .datasim_event(
    params = as.list(match.call())[-1],
    label = label
  )
}



#' No specified event distribution
#'
#' @return `null_event_dist` returns an object with no parameters specified that does not simulate event times.
#' @export
#' @rdname create_event_dist
#'
#' @examples
#' null_event_dist()
null_event_dist <- function() {
  .datasim_event(
    params = list(),
    label = "No distribution specified"
  )
}

# Specify Enrollment into Trial --------------

#' Enrollment Object
#'
#' @slot fun A function that takes one argument `n` the number of enrollment times to observe and returns a
#'   vector of times.
#' @slot label A user-friendly label
.datasim_enrollment <- setClass(
  "DataSimEnrollment",
  slots = c(
    fun = "function",
    label = "character"
  ),
  prototype = list(
    fun = function(n) rep(1, n),
    label = "Enrolling 1 patient per time"
  )
)

#' Create a `DataSimEnrollment` Object
#'
#' @param fun A function that takes one argument `n` the number of enrollment times to observe and returns a
#'   vector of times.
#' @param label A user-friendly label
#'
#' @return A [DataSimEnrollment][DataSimEnrollment-class] object
#' @export
#'
#' @examples
#' custom_enrollment(
#'   fun = function(n) rpois(n, lambda = 5),
#'   label = "Poisson enrollment distribution"
#' )
custom_enrollment <- function(fun, label) {
  assert_string(label)
  assert_function(fun, args = "n")
  .datasim_enrollment(fun = fun, label = label)
}

#' Constant Enrollment Rates
#'
#' @param rate Number of patients to enroll per unit time
#' @param for_time Number of time periods for each rate. Must be equal length to `rate`
#'
#' @return An object of class [DataSimEnrollment][DataSimEnrollment-class] to be passed to [create_data_simulation()]
#'
#' @export
#' @examples
#' # 10 patients/month for 6 months, then 5/month for 6 months
#' enroll_obj <- enrollment_constant(rate = c(10, 5), for_time = c(6, 6))
#' enroll_obj@fun(n = 80)
enrollment_constant <- function(rate, for_time = rep(1, length(rate))) {
  assert_integerish(rate, min.len = 1)
  assert_integerish(for_time, len = length(rate))
  .datasim_enrollment(
    fun = function(n) {
      enrolled_per_t <- rep(rate, times = for_time)
      enrollment_times <- rep(seq_along(enrolled_per_t), times = enrolled_per_t)
      if (length(enrollment_times) < n) {
        stop("Not enough patients could be enrolled. Revise the enrollment rates and times.")
      }
      enrollment_times[seq_len(n)]
    },
    label = paste("Enrolling patients per time at rates:", toString(rep(rate, times = for_time), width = 50))
  )
}

#' Set Enrollment Rates for Internal and External Trials
#'
#' @param object A `DataSimObject` from [create_data_simulation]
#' @param internal `DataSimEnrollment` object to define the enrollment times for internal data
#' @param external `DataSimEnrollment` object to define the enrollment times for external data. Defaults to be the same
#' as internal.
#'
#' @return A `DataSimObject` with updated `enrollment_internal` and `enrollment_external` slots.
#' @export
#' @examples
#' data_sim <- create_data_simulation(
#'   create_baseline_object(10, 10, 10),
#'   event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' set_enrollment(
#'   data_sim,
#'   internal = enrollment_constant(rate = c(10, 5), for_time = c(6, 6)),
#'   external = enrollment_constant(rate = c(5), for_time = c(20))
#' )
set_enrollment <- function(object, internal, external = internal) {
  assert_class(object, "DataSimObject")
  assert_class(internal, "DataSimEnrollment")
  assert_class(external, "DataSimEnrollment")
  object@enrollment_internal <- internal
  object@enrollment_external <- external
  object
}


# Specify Clinical Cut Offs ----------

#' Cut Off Functions
#'
#' @return A `DataSimCutOff` object containing a cut-off function
#' @name cut_off_funs
#' @rdname cut_off_funs
NULL

#' @export
#' @describeIn cut_off_funs No cut off is specified
#' @examples
#' cut_off_none()
cut_off_none <- function() {
  .datasim_cut_off(
    fun = function(data) {
      data
    },
    label = "No cut off"
  )
}

#' @param time Time to cut off
#' @export
#' @describeIn cut_off_funs Cut off at `time` after first enrolled patient
#' @examples
#' cut_off_after_first(time = 36)
cut_off_after_first <- function(time) {
  .datasim_cut_off(
    fun = function(data) {
      cut_time <- min(data$enrollment) + time
      after_cut_off <- data$enrollment + data$eventtime > cut_time
      data$status <- ifelse(after_cut_off, 0, data$status)
      data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, data$eventtime)
      data[data$enrollment < cut_time, ]
    },
    label = h_glue("Cut off after first enrolled patient reaches time = {{time}}")
  )
}

#' @param time Time to cut off
#' @export
#' @describeIn cut_off_funs Cut off at `time` after last enrolled patient
#' @examples
#' cut_off_after_last(time = 36)
cut_off_after_last <- function(time) {
  .datasim_cut_off(
    fun = function(data) {
      cut_time <- max(data$enrollment) + time
      after_cut_off <- data$enrollment + data$eventtime > cut_time
      data$status <- ifelse(after_cut_off, 0, data$status)
      data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, data$eventtime)
      data
    },
    label = h_glue("Cut off after last enrolled patient reaches time={{time}}")
  )
}

#' @param n Number of events
#' @export
#' @describeIn cut_off_funs Cut off after the time of the n-th event
#' @examples
#' cut_off_after_events(n = 20)
cut_off_after_events <- function(n) {
  .datasim_cut_off(
    fun = function(data) {
      cut_time <- sort(data$enrollment + data$eventtime)[n]
      after_cut_off <- data$enrollment + data$eventtime > cut_time
      data$status <- ifelse(after_cut_off, 0, data$status)
      data$eventtime <- ifelse(after_cut_off, cut_time - data$enrollment, data$eventtime)
      data[data$enrollment < cut_time, ]
    },
    label = h_glue("Cut off after {{n}} events")
  )
}

#' Cut Off Object
#' @slot cut_off_fun A function that takes a `data.frame` with columns of enrollment time, survival time and outcome.
#' The function returns a modified `data.frame` after applied the cut-off rule.
.datasim_cut_off <- setClass(
  "DataSimCutOff",
  slots = c(
    fun = "function",
    label = "character"
  ),
  prototype = list(
    fun = function(data) data,
    label = "No cut off"
  )
)

#' Set Clinical Cut Off Rule
#'
#' @param object `DataSimObject`
#' @param internal `DataSimCutOff` object specified by one of the cut off functions: `cut_off_after_events()`,
#' `cut_off_after_first()`, `cut_off_after_last()`, `cut_off_none()`.
#' @param external `DataSimCutOff` for the external data.
#'
#' @return A `DataSimObject` with updated `cut_off_internal` and `cut_off_external` slots.
#' @export
#'
#' @examples
#' data_sim <- create_data_simulation(
#'   create_baseline_object(10, 10, 10),
#'   event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' set_cut_off(
#'   data_sim,
#'   cut_off_after_events(n = 10),
#'   cut_off_after_first(time = 30)
#' )
set_cut_off <- function(object, internal = cut_off_none(), external = cut_off_none()) {
  assert_class(object, "DataSimObject")
  assert_class(internal, "DataSimCutOff")
  assert_class(external, "DataSimCutOff")
  object@cut_off_internal <- internal
  object@cut_off_external <- external
  object
}

# Specify Drop Out Rates -------------

#' Set Drop Out Distribution
#'
#' @param object `DataSimObject`
#' @param internal_treated `DataSimEvent` object specifying distribution for internal treated patients.
#' @param internal_control `DataSimEvent` object specifying distribution for internal control patients.
#' @param external_control `DataSimEvent` object specifying distribution for external control patients.
#'
#' @details `DataSimEvent` objects can be specified with [create_event_dist()]. Currently no `beta` parameters can be
#' used in drop out distributions (unlike for the survival outcome).
#'
#' @return  A `DataSimObject` with updated `internal_treated`, `internal_control` and `external_control` slots.
#' @export
#'
#' @examples
#' data_sim <- create_data_simulation(
#'   create_baseline_object(10, 10, 10),
#'   event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' set_dropout(
#'   data_sim,
#'   internal_treated = create_event_dist(dist = "exponential", lambdas = 1 / 55),
#'   internal_control = create_event_dist(dist = "exponential", lambdas = 1 / 50),
#'   external_control = create_event_dist(dist = "exponential", lambdas = 1 / 40)
#' )
set_dropout <- function(object,
                        internal_treated,
                        internal_control,
                        external_control) {
  assert_class(object, "DataSimObject")
  assert_class(internal_treated, "DataSimEvent")
  assert_class(internal_control, "DataSimEvent")
  assert_class(external_control, "DataSimEvent")
  object@dropout_internal_treated <- internal_treated
  object@dropout_internal_control <- internal_control
  object@dropout_external_control <- external_control
  object
}


# Data Simulation -----------------

#' Data Simulation Object Class
#'
#' @slot baseline `BaselineObject` from [create_baseline_object]
#' @slot coefficients Named `numeric` vector of `beta` coefficients for survival model. See `beta` at
#'   `?simsurv::simsurv`
#' @slot treatment_hr `numeric` treatment effect as a hazard ration. `log(treatment_hr)` is included in `beta` with
#'   `coefficients` and `log(drift_hr)`. This default is overridden by [generate][generate,DataSimObject-method]
#'   arguments
#' @slot drift_hr `numeric` hazard ratio between internal and external arms. Included as `log(drift_hr)`.
#' @slot fixed_external_data `data.frame` for external data. Currently unused.
#' @slot event_dist `DataSimEvent` parameters for outcome distribution from [create_event_dist()]
#' @slot enrollment `DataSimEnrollment` object.
#' @slot cut_off `DataSimCutOff`
#'
#' @return A `DataSimObject`
.datasim_object <- setClass(
  "DataSimObject",
  slots = c(
    baseline = "BaselineObject",
    coefficients = "numeric",
    treatment_hr = "numeric",
    drift_hr = "numeric",
    fixed_external_data = "DataSimFixedExternalData",
    event_dist = "DataSimEvent",
    enrollment_internal = "DataSimEnrollment",
    enrollment_external = "DataSimEnrollment",
    cut_off_internal = "DataSimCutOff",
    cut_off_external = "DataSimCutOff",
    dropout_internal_treated = "DataSimEvent",
    dropout_internal_control = "DataSimEvent",
    dropout_external_control = "DataSimEvent"
  )
)


#' Data Simulation
#'
#' @param baseline `BaselineObject` from [create_baseline_object()]
#' @param coefficients Named vector of coefficients for linear predictor. Must correspond to variables in baseline
#'   object
#' @param treatment_hr Default treatment hazard ratio for simulations. Alternative simulation settings can be specified
#'   in [generate].
#' @param drift_hr Default drift hazard ratio between internal and external arms. Alternative simulation settings can be
#'   specified in [generate].
#' @param event_dist Specify time to event distribution with `SimDataEvent` object from [create_event_dist()]
#' @param fixed_external_data A `data.frame` containing external control data. It must contain columns `eventtime`,
#'   `status` and all of the variables named in `coefficients`. If present, `trt` must be 0 and `ext` must be 1 for all
#'   rows.
#'
#' @return `DataSimObject`
#' @export
#'
#' @examples
#' baseline_obj <- create_baseline_object(
#'   n_trt_int = 100,
#'   n_ctrl_int = 50,
#'   n_ctrl_ext = 10,
#'   covariates = baseline_covariates(
#'     names = c("age", "score"),
#'     means_int = c(55, 5),
#'     means_ext = c(60, 5),
#'     covariance_int = covariance_matrix(c(5, 1))
#'   )
#' )
#' sim_obj <- create_data_simulation(
#'   baseline_obj,
#'   coefficients = c(age = 0.001, score = 1.5),
#'   event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' data_sim_list <- generate(sim_obj, treatment_hr = c(0.5, 1), drift_hr = 0.5)
create_data_simulation <- function(baseline,
                                   coefficients = numeric(),
                                   treatment_hr = 1,
                                   drift_hr = 1,
                                   event_dist,
                                   fixed_external_data) {
  assert_class(baseline, "BaselineObject")
  assert_numeric(coefficients, finite = TRUE, names = "named", min.len = 0)
  assert_numeric(treatment_hr, finite = TRUE, len = 1, any.missing = FALSE)
  assert_numeric(drift_hr, finite = TRUE, len = 1, any.missing = FALSE)

  possible_coefs <- possible_data_sim_vars(baseline)
  unknown_names <- setdiff(names(coefficients), possible_coefs)
  if (length(unknown_names)) {
    stop(
      "Unknown coefficient specified: ", toString(unknown_names), "\n",
      "Coefficient names must match: ", toString(possible_coefs),
      call. = FALSE
    )
  }

  if (!missing(fixed_external_data)) {
    fixed_data_object <- check_fixed_external_data(fixed_external_data, names(coefficients))
  } else {
    fixed_data_object <- .datasim_fixed_external_data()
  }

  .datasim_object(
    baseline = baseline,
    coefficients = coefficients,
    treatment_hr = treatment_hr,
    drift_hr = drift_hr,
    event_dist = event_dist,
    fixed_external_data = fixed_data_object
  )
}

#' Helper function to create one arm dataset
#'
#' @param baseline Baseline data
#' @param betas Coefficients for outcome model, including treatment and drift
#' @param event_dist `DataSimEvent` Parameters for [simsurv::simsurv] for outcome model
#' @param enrollment `DataSimEnrollment` object to create enrollment times
#' @param dropout `DataSimEvent` object to generate dropout times
#'
#' @return A `data.frame`
#' @noRd
make_one_dataset <- function(baseline, betas, event_dist, enrollment, dropout) {
  # skip if there are no patients in this arm
  if (nrow(baseline) == 0) {
    return(data.frame())
  }

  data <- data.frame(
    patid = baseline$patid,
    model.matrix(
      object = as.formula(paste(c("~ 0", names(betas)), collapse = "+")),
      data = data.frame(baseline)
    )
  )

  # Generate outcome event times
  surv_df <- tryCatch(
    do.call(simsurv::simsurv, args = c(list(betas = betas, x = data), event_dist@params)),
    error = function(e) {
      cat("Error caught when generating survival times. Check parameters:\n")
      print(event_dist@params)
      stop(e)
    }
  )

  data$eventtime <- surv_df$eventtime
  data$status <- surv_df$status

  # Generate drop out times
  if (length(dropout@params)) {
    drop_df <- tryCatch(
      do.call(simsurv::simsurv, args = c(dropout@params, list(x = data))),
      error = function(e) {
        cat("Error caught when generating drop out times. Check parameters:\n")
        print(dropout@params)
        stop(e)
      }
    )
    drop_flag <- data$eventtime < drop_df$eventtime
    data$eventtime <- ifelse(drop_flag, drop_df$eventtime, data$eventtime)
    data$status <- ifelse(drop_flag, 0, data$status)
  }

  # Calculate enrollment for generated observations
  data$enrollment <- sample(enrollment@fun(nrow(data)))
  if (any(data$enrollment < 0)) {
    warning(
      "Negative enrollment times were generated. ",
      "Check the @enrollment_internal and @enrollment_external slots if this is unexpected."
    )
  }

  data
}

#' @importFrom generics generate
# nolint start
generate.DataSimObject <- function(x, n = 1, treatment_hr = NULL, drift_hr = NULL) {
  # nolint end

  if (is.null(treatment_hr)) treatment_hr <- x@treatment_hr
  if (is.null(drift_hr)) drift_hr <- x@drift_hr
  if (!all(drift_hr == 1) && x@fixed_external_data@n > 0) {
    warning("Drift parameter is not applied to fixed external data", call. = FALSE)
  }

  guide <- expand.grid(treatment_hr = treatment_hr, drift_hr = drift_hr)
  guide <- cbind(sim_id = seq_len(nrow(guide)), guide)

  simulated_data <- list()
  for (i in seq_len(nrow(guide))) {
    betas <- c(
      x@coefficients,
      trt = log(guide$treatment_hr[i]),
      ext = log(guide$drift_hr[i])
    )

    simulated_data[[i]] <- replicate(n, simplify = FALSE, expr = {
      # generate baseline data
      df_list <- generate(x@baseline)

      df_list <- .mapply(
        FUN = make_one_dataset,
        dots = list(
          baseline = df_list,
          enrollment = list(x@enrollment_internal, x@enrollment_internal, x@enrollment_external),
          dropout = list(x@dropout_internal_treated, x@dropout_internal_control, x@dropout_external_control)
        ),
        MoreArgs = list(
          betas = betas,
          event_dist = x@event_dist
        )
      )

      if (x@fixed_external_data@n > 0) {
        x@fixed_external_data@data$patid <- seq_len(x@fixed_external_data@n) + sum(sapply(df_list, nrow))
        missing_cols <- setdiff(colnames(df_list[[1]]), colnames(x@fixed_external_data@data))
        if (length(missing_cols)) {
          warning("Missing columns in fixed external data: ", toString(missing_cols), call. = FALSE)
          x@fixed_external_data@data[, missing_cols] <- NA
        }
      }

      # Apply clinical cut off
      df <- rbind(
        x@cut_off_internal@fun(rbind(df_list[[1]], df_list[[2]])),
        x@cut_off_external@fun(df_list[[3]]),
        x@fixed_external_data@data
      )
      df$cens <- 1 - df$status
      as.matrix(df)
    })
  }
  sim_data_list(
    data_list = simulated_data,
    guide = guide,
    effect = "treatment_hr",
    drift = "drift_hr",
    index = "sim_id"
  )
}


#' Generate Data for a `DataSimObject`
#'
#' @param x a `DataSimObject` object created by [create_data_simulation]
#' @param n number of data sets to simulate
#' @param treatment_hr vector of numeric treatment effects
#' @param drift_hr vector of numeric drift effects
#'
#' @return A [SimDataList][SimDataList-class] object for use with [create_simulation_obj()].
#' @export
#'
#' @examples
#' baseline_obj <- create_baseline_object(
#'   n_trt_int = 100,
#'   n_ctrl_int = 50,
#'   n_ctrl_ext = 10,
#'   covariates = baseline_covariates(
#'     names = c("age", "score"),
#'     means_int = c(55, 5),
#'     means_ext = c(60, 5),
#'     covariance_int = covariance_matrix(c(5, 1))
#'   )
#' )
#' sim_obj <- create_data_simulation(
#'   baseline_obj,
#'   coefficients = c(age = 0.001, score = 1.5),
#'   event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' data_sim_list <- generate(sim_obj, treatment_hr = c(0, 1), drift_hr = 0.5)
setMethod(
  f = "generate",
  signature = "DataSimObject",
  definition = generate.DataSimObject
)

setMethod(
  f = "show",
  signature = "DataSimObject",
  definition = function(object) {
    cat("DataSimObject\n")
    cat("-------------\n")
    cat("Baseline object:\n")
    print(object@baseline)
    cat("\n")

    cat("Event distribution:\n")
    cat(object@event_dist@label, "\n")
    cat("\n")

    cat("Treatment HR: ", toString(object@treatment_hr), "\n")
    cat("Drift HR: ", toString(object@drift_hr), "\n")
    cat("\n")

    if (length(object@coefficients) > 0) {
      cat("Coefficients:\n")
      print(object@coefficients)
      cat("\n")
    }

    cat("Enrollment:\n")
    cat(" Internal:", object@enrollment_internal@label, "\n")
    cat(" External:", object@enrollment_external@label, "\n")
    cat("\n")

    cat("Dropout:\n")
    cat(" Internal treated:", object@dropout_internal_treated@label, "\n")
    cat(" Internal control:", object@dropout_internal_control@label, "\n")
    cat(" External control:", object@dropout_external_control@label, "\n")
    cat("\n")

    cat("Clinical cut off:\n")
    cat(" Internal:", object@cut_off_internal@label, "\n")
    cat(" External:", object@cut_off_external@label, "\n")
    cat("\n")

    if (object@fixed_external_data@n > 0) {
      cat("Fixed external data:\n")
      cat(" Columns:", toString(colnames(object@fixed_external_data@data)), "\n")
      cat(" N:", object@fixed_external_data@n, "\n")
      cat("\n")
    }
  }
)
