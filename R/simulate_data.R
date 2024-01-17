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
#'
#' @return A `DataSimEvent`
.datasim_event <- setClass(
  "DataSimEvent",
  slots = c(
    params = "list"
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
#' weibull_surv <- event_dist(dist = "weibull", lambdas = 1 / 200, gammas = 1)
#' exp_event_dist <- event_dist(dist = "exponential", lambdas = 1 / 36)
event_dist <- function(dist = NULL,
                       lambdas = NULL,
                       gammas = NULL,
                       mixture = FALSE,
                       pmix = 0.5,
                       hazard = NULL,
                       loghazard = NULL,
                       cumhazard = NULL,
                       logcumhazard = NULL,
                       ...) {
  .datasim_event(
    params = as.list(match.call())[-1]
  )
}


# Specify Enrollment into Trial --------------

#' Enrollment Object
#' @slot enrollment_fun A function that takes one argument `n` the number of enrollment times to observe and returns a
#'   vector of times.
.datasim_enrollment <- setClass(
  "DataSimEnrollment",
  slots = c(
    fun = "function"
  ),
  prototype = list(
    fun = function(n) rep(1, n)
  )
)

#' Fixed Enrollment Rates
#'
#' @param rate Number of patients to enroll per unit time
#' @param for_time Number of time periods for each rate. Must be equal length to `rate`
#'
#' @return An object of class [DataSimEnrollment][DataSimEnrollment-class] to be passed to [create_data_simulation()]
#'
#' @export
#' @examples
#' # 10 patients/month for 6 months, then 5/month for 6 months
#' enroll_obj <- enrollment_fixed(rate = c(10, 5), for_time = c(6, 6))
#' enroll_obj@fun(n = 80)
enrollment_fixed <- function(rate, for_time = rep(1, length(rate))) {
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
    }
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
#'   event_dist = event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' set_enrollment(
#'   data_sim,
#'   internal = enrollment_fixed(rate = c(10, 5), for_time = c(6, 6)),
#'   external = enrollment_fixed(rate = c(5), for_time = c(20))
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
    }
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
      data$eventtime <- ifelse(after_cut_off, cut_time, data$eventtime)
      data[data$enrollment < cut_time, ]
    }
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
      data$eventtime <- ifelse(after_cut_off, cut_time, data$eventtime)
      data
    }
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
      data$eventtime <- ifelse(after_cut_off, cut_time, data$eventtime)
      data[data$enrollment < cut_time, ]
    }
  )
}

#' Cut Off Object
#' @slot cut_off_fun A function that takes a `data.frame` with columns of enrollment time, survival time and outcome.
#' The function returns a modified `data.frame` after applied the cut-off rule.
.datasim_cut_off <- setClass(
  "DataSimCutOff",
  slots = c(
    fun = "function"
  ),
  prototype = list(
    fun = function(data) data
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
#'   event_dist = event_dist(dist = "exponential", lambdas = 1 / 36)
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
#' @details
#' `DataSimEvent` objects can be specified with [event_dist]. Currently no `beta` parameters can be used in drop out
#' distributions (unlike for the survival outcome).
#'
#' @return  A `DataSimObject` with updated `internal_treated`, `internal_control` and `external_control` slots.
#' @export
#'
#' @examples
#' data_sim <- create_data_simulation(
#'   create_baseline_object(10, 10, 10),
#'   event_dist = event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' set_dropout(
#'   data_sim,
#'   internal_treated = event_dist(dist = "exponential", lambdas = 1 / 55),
#'   internal_control = event_dist(dist = "exponential", lambdas = 1 / 50),
#'   external_control = event_dist(dist = "exponential", lambdas = 1 / 40)
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
#' @slot treatment_effect `numeric` treatment effect used as a `beta` with `coefficients` and `drift`. This default is
#'   overridden by [generate.DataSimObject] arguments
#' @slot drift `numeric` difference between internal and external arms
#' @slot fixed_external_data `data.frame` for external data. Currently unused.
#' @slot event_dist `DataSimEvent` parameters for outcome distribution from [event_dist()]
#' @slot enrollment `DataSimEnrollment` object.
#' @slot cut_off `DataSimCutOff`
#'
#' @return A `DataSimObject`
.datasim_object <- setClass(
  "DataSimObject",
  slots = c(
    baseline = "BaselineObject",
    coefficients = "numeric",
    treatment_effect = "numeric",
    drift = "numeric",
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
#' @param coefficients Named vector of coefficients for linear predictor.
#' Must correspond to variables in baseline object
#' @param treatment_effect Treatment effect coefficient on linear predictor scale.
#' @param drift Drift parameter between internal and external arms on linear predictor scale
#' @param event_dist Specify time to event distribution with `SimDataEvent` object from [event_dist()]
#' @param fixed_external_data Currently ignored.
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
#'   event_dist = event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' data_sim_list <- generate(sim_obj, treatment_effect = c(0, 1), drift = 0.5)
create_data_simulation <- function(baseline,
                                   coefficients = numeric(),
                                   treatment_effect = 0,
                                   drift = 0,
                                   event_dist,
                                   fixed_external_data) {
  assert_class(baseline, "BaselineObject")
  assert_numeric(coefficients, finite = TRUE, names = "named", min.len = 0)
  assert_numeric(treatment_effect, finite = TRUE)
  assert_numeric(drift, finite = TRUE)

  possible_coefs <- possible_data_sim_vars(baseline)
  unknown_names <- setdiff(names(coefficients), possible_coefs)
  if (length(unknown_names)) {
    stop(
      "Unknown coefficient specified: ", toString(unknown_names), "\n",
      "Coefficient names must match: ", toString(possible_coefs),
      call. = FALSE
    )
  }

  if (FALSE) {
    if (!missing(fixed_external_data)) {
      assert_data_frame(fixed_external_data, min.rows = 1)
      fixed_data_object <- check_fixed_external_data(fixed_external_data, coefficients)
    } else {
      fixed_data_object <- .datasim_fixed_external_data()
    }
  }

  ds <- .datasim_object(
    baseline = baseline,
    coefficients = coefficients,
    treatment_effect = treatment_effect,
    drift = drift,
    event_dist = event_dist,
    fixed_external_data = .datasim_fixed_external_data()
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

  data
}

#' @importFrom generics generate
# nolint start
generate.DataSimObject <- function(x, n = 1, treatment_effect = NULL, drift = NULL) {
  # nolint end

  if (is.null(treatment_effect)) treatment_effect <- x@treatment_effect
  if (is.null(drift)) drift <- x@drift

  guide <- expand.grid(treatment_effect = treatment_effect, drift = drift)
  guide <- cbind(sim_id = seq_len(nrow(guide)), guide)

  simulated_data <- list()
  for (i in seq_len(nrow(guide))) {
    betas <- c(
      x@coefficients,
      trt = guide$treatment_effect[i],
      ext = guide$drift[i]
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
      # Apply clinical cut off
      df <- rbind(
        x@cut_off_internal@fun(rbind(df_list[[1]], df_list[[2]])),
        x@cut_off_external@fun(df_list[[3]])
      )
      df$patid <- seq_len(nrow(df))
      as.matrix(df)
    })
  }
  list(
    guide = guide,
    data_list = simulated_data
  )
}


#' Generate Data for a `DataSimObject`
#'
#' @param x a `DataSimObject` object created by [create_data_simulation]
#' @param n number of data sets to simulate
#' @param treatment_effect vector of numeric treatment effects
#' @param drift vector of numeric drift effects
#'
#' @return A list of list of matrices
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
#'   event_dist = event_dist(dist = "exponential", lambdas = 1 / 36)
#' )
#' data_sim_list <- generate(sim_obj, treatment_effect = c(0, 1), drift = 0.5)
setMethod(
  f = "generate",
  signature = "DataSimObject",
  definition = generate.DataSimObject
)
