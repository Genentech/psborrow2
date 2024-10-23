
#' Set control parameters for Case Weights
#'
#' @param boxp_dist Which method to use for Box P distribution calculation:
#' `"kde"`: Sampling and density with `ks::kde`
#' `"density"`: Sampling and density with `stats::density`
#' `"W"`: Solve using Lambert W function
#'
#' @return
#' @export
#'
#' @examples
control_pars <- function(boxp_dist = "W", samples_1 = 100, samples_2 = 100) {
  list(
    boxp_dist = boxp_dist,
    samples_1 = samples_1,
    samples_2 = samples_2
  )
}

#' Choose split times based on quatiles of times
#'
#' @param times Times to split
#' @param n Number of intervals, must be $\ge 2$
#'
#' @return A numeric vector of split times
#'
#' @examples
#' equal_splits(rexp(100, rate = 1/30), 4)
equal_splits <- function(times, n) {
  assert_integerish(n, lower = 2)
  assert_numeric(times)
  breaks <- seq(0, 1, length = n + 1)[-c(1, n + 1)]
  quantile(times, probs = breaks)
}


#' Split data at cut times
#'
#' @param data A data frame
#' @param time_var The name of the time varianlte
#' @param event_var The name of the event variables
#' @param cuts The cut times
#'
#' @return A split data.frame with additional columns
#' `"interval"`, `"expo"`, `"t0"`
#' @export
#'
#' @examples
split_data <- function(data, time_var, event_var, cuts) {
  sim.split <- survival::survSplit(
    data,
    cut = unique(c(0,cuts)),
    end = time_var,
    start = "t0",
    event = event_var,
    episode = "interval"
  )
  sim.split$expo <- sim.split[[time_var]] - sim.split$t0

  sim.split
}


#' Calculate Case Weights
#'
#' @param sim.data data.frame with 1 row per patient
#' @param idx_n Number of samples from asymptotic distribution
#' @param cuts Specified cut times. One of `cuts` or `n_intervals` must be specified. `cuts` takes precedence if specified.
#' @param n_intervals Number of cut times. If only `n_intervals` is specified, the times where events occur will be
#'  divided into equally size groups.
#' @param cov.names Covariates to use in models
#' @param event.var Column containing events (0/1)
#' @param external.var
#' @param samples Number of samples from current and historical models. Integer vector with length 1 or 2.
#' @param control_pars Set the control paramers
#'
#' @return A matrix of weights
#' @export
#'
#' @examples
#' res <- case_weights(
#'  sim.data, cuts = 3, event.var = "nu", time.var = "t",
#'  treat.var = "treat", cov.names = c("age", "sex2"), samples = c(7, 5),
#'  control_pars = control_pars()
#'  )
case_interval_weights <- function(sim.data,
                         cuts = NULL,
                         n_intervals = NULL,
                         event.var,
                         time.var,
                         treat.var,
                         external.var,
                         cov.names,
                         pars = control_pars()) {
  if (is.null(cuts)) {
    if (is.null(n_intervals)) {
      stop("One of `cuts` or `n_intervals` must be defined")
    } else if (!is.null(n_intervals)) {
      cuts <- equal_splits(sim.data[sim.data[[event.var]] == 1, time.var], n_intervals)
    }
  }
  assert_numeric(setdiff(cuts, 0), min.len = 1)
  cut.time.mod <- unique(c(0, cuts))
  n.seg <- length(cut.time.mod)
  interval.names <- paste0("interval", seq_along(cut.time.mod))

  inner.t <- setdiff(cut.time.mod, 0)
  diff <- diff(cut.time.mod)

  sim.split <- split_data(
    data = sim.data,
    time_var = time.var,
    event_var = event.var,
    cuts = cut.time.mod
  )

  base_formula <- reformulate(cov.names, response = event.var)


  sim.split$interval <- factor(sim.split$interval - 1)
  # Fit model for historical events
  # formula2_no_trt <- formula(paste(event.var, "~ ", paste(cov.names, collapse = " + "), " + interval + offset(log(expo)) - 1"))
  formula2_no_trt <- update(base_formula, ~ . + interval + offset(log(expo)) - 1)
  sim.poisson <- fit_poisson_glm(formula2_no_trt, sim.split[sim.split[[external.var]] == 1, ])

  # Fit model for RCT events
  formula2 <- update(base_formula, ~ . + interval + offset(log(expo)) - 1)
  sim.poisson.lambda <- fit_poisson_glm(formula2, sim.split[sim.split[[external.var]] == 0, ])

  # } else {
  #   # Fit model for historical events
  #   # formula1_no_trt <- formula(paste(event.var, "~ ", paste(cov.names, collapse = " + "), " + offset(log(expo))"))
  #   formula1_no_trt <-  update(base_formula, ~ . + offset(log(expo)))
  #   sim.poisson <- fit_poisson_glm(formula1_no_trt, sim.split[sim.split[[external.var]] == 1, ])
  #   names(sim.poisson$coefficients)[names(sim.poisson$coefficients) == "(Intercept)"] <- "interval1"
  #
  #   # Fit model for RCT events
  #   formula1 <- update(base_formula, paste0("~ . + ", treat.var, " + offset(log(expo))"))
  #   sim.poisson.lambda <- fit_poisson_glm(formula2_no_trt, sim.split[sim.split[["external.var"]], ])
  #   names(sim.poisson.lambda$coefficients)[names(sim.poisson.lambda$coefficients) == "(Intercept)"] <- "interval1"
  # }

  # same for censoring models -------
browser()
  # Fit model for historical censoring
  # if (n.seg >= 3) {
  sim.poisson.theta <- fit_poisson_glm(
    paste0("1 - ", treat.var, " ~ interval + offset(log(expo)) - 1"),
    sim.split[sim.split[[external.var]] == 1, ]
  )
  # } else {
  #   sim.poisson.theta <- fit_poisson_glm(
  #     paste0("1 - ", treat.var, " ~ offset(log(expo))"),
  #     sim.split[sim.split$external == 1, ]
  #   )
  #   names(sim.poisson.theta$coefficients)[names(sim.poisson.theta$coefficients) == "(Intercept)"] <- "interval1"
  # }

  calculate_case_weights(
    sim.poisson,
    sim.poisson.lambda,
    sim.poisson.theta,
    cov.names,
    interval.names,
    sim.split,
    diff,
    pars = pars
  )
  # do_thing(
  #   sim.poisson, sim.poisson.lambda, sim.poisson.theta, cov.names, interval.names, n.seg, sim.split,
  #   a0.outer,  inner.t, diff,
  #   control_pars
  # )

}




#' Fit a poisson GLM
fit_poisson_glm <- function(formula, data) {
  glm(
    formula,
    family = poisson(link = "log"),
    data = data
  )
}

# Sample coefs from GLM
sample_coefs <- function(model, covs) {
  coef.model <- model$coefficients
  cov.index <- names(coef.model) %in% c(covs)

  mvrnorm(1, coef.model[cov.index], vcov(model)[cov.index, cov.index])
}

# Some magic functions rotate3
rotate3 <- function(x, p) {
  2^(p - 1) * sign(x - 0.5) * abs(x - 0.5)^p + 0.5
}

# Some magic function g
g <- function(x, q) {
  1 / (1 + exp(-(200 * (x - q))))
}


#' Cumulative Probability of X ~ log(Y), where Y ~ Exp(rate)
#'
#' @param x value to evaluate probability
#' @param rate lambda parameter
#'
#' @return probability values
plog_exp <- function(x, rate) {
  1 - exp(-rate * exp(x))
}


#' Density of X ~ log(Y), where Y ~ Exp(rate)
#' @param x value to evaluate density
#' @param rate lambda parameter
#'
#' @return density values
dlog_exp <- function(x, rate) {
  rate * exp(x - rate * exp(x))
  # consider log
  # exp(
  #   log(rate) + x - rate*exp(x)
  # )
}

#' #' Log Piecewise Exponential distribution
#' plog_pcexp <- function(x, rate) {
#'   1 - exp(-rate * exp(x))
#' }
#'
#' dlog_pcexp <- function(x, rate, time) {
#'   rate * exp(x - rate * exp(x))
#'   # consider log
#'   # exp(
#'   #   log(rate) + x - rate*exp(x)
#'   # )
#' }

# Find x values where pdf(x|rate) == pdf(a|rate)
critical_values <- function(a, rate = this_rate) {
  matrix(
    log(a / rate) - c(lamW::lambertW0(-a), lamW::lambertWm1(-a)),
    nrow = length(a)
  )
}


# Get boxp
get_boxp_W <- function(a, lambda_t, lambda_c) {
  rate <- as.numeric(lambda_t + lambda_c)
  pdf_vals <- dlog_exp(a, rate) |>
    critical_values(rate = rate) |>
    plog_exp(rate = rate)
  1 - pdf_vals[, 2] + pdf_vals[, 1]
}

# Get interval
# get_boxp_interval <- function(a, lambda_t, lambda_c) {
#   rate <- as.numeric(lambda_t + lambda_c)
#   pdf_vals <- dlog_exp(a, rate) |>
#     critical_values(rate = rate) |>
#     plog_exp(rate = rate)
#   1 - pdf_vals[, 2] + pdf_vals[, 1]
# }


get_boxp_density <- function(a, lambda_t, lambda_c) {
  mapply(
    a = a,
    lambda_t = lambda_t,
    lambda_c = lambda_c,
    FUN = function(a, lambda_t, lambda_c) {
      t.h.pred <- rexp(1E4, rate = lambda_t)
      c.h.pred <- rexp(1E4, rate = lambda_c)
      y.h.pred <- pmin(t.h.pred, c.h.pred)

      dens <- density(log(y.h.pred))
      dens_fun <- approxfun(dens$x, dens$y)
      fhat_predict_outer_d <- dens_fun(log(y.h.pred))
      mean(fhat_predict_outer_d <= dens_fun(a))
    }
  )
}


get_boxp_kde <- function(a, lambda_t, lambda_c) {
  mapply(
    a = a,
    lambda_t = lambda_t,
    lambda_c = lambda_c,
    FUN = function(a, lambda_t, lambda_c) {
      t.h.pred <- rexp(1E4, rate = lambda_t)
      c.h.pred <- rexp(1E4, rate = lambda_c)
      y.h.pred <- pmin(t.h.pred, c.h.pred)

      fhat_outer <- kde(log(y.h.pred))
      fhat_predict_outer <- predict(fhat_outer, x = log(y.h.pred))

      mean(fhat_predict_outer <= predict(fhat_outer, x = a))
    }
  )
}

calculate_case_weights <- function(sim.poisson,
                                   sim.poisson.lambda,
                                   sim.poisson.theta,
                                   cov.names,
                                   interval.names,
                                   sim.split,
                                   diff,
                                   pars) {

  sim.split.ex <- sim.split[sim.split$external == 1, ]
  sim.mm <- model.matrix(sim.poisson) # model matrix with external == 1 and covariates

  interval.names <- names(sim.poisson.theta$coefficients)
  cov.names <- names(sim.poisson.lambda$coefficients) |> grep(pattern = "interval", invert = TRUE, value = TRUE)

  iteration_fun <- function(sim.split.ex, sim.mm) {
    # one sample of lambda from model of outcomes in historical data
    lambda <- sample_coefs(sim.poisson, c(cov.names, interval.names))

    # one sample theta from historical censoring model
    theta <- sample_coefs(sim.poisson.theta, interval.names)

    ### Loop 2) (idx) Add imputation for multiple intervals in historical controls
    sim.split.idx <- sim.split.ex # 2021-08-03

    # Identify rows which are alive and uncensored at the end of the period:
    impute_these_id <- sim.split.idx$nu == 0 & sim.split.idx$expo == diff[sim.split.idx$interval]
    rate_t <- exp(sim.mm[impute_these_id,] %*% lambda)
    rate_c <- exp(sim.mm[impute_these_id, interval.names] %*% theta)

    temp.y <- rexp(rep(1, sum(impute_these_id)), rate_t + rate_c)
    sim.split.idx$expo[impute_these_id] <- sim.split.idx$expo[impute_these_id] + temp.y

    # subset to historical only
    idx_a0_n <- control_pars$samples_2 # default 5? Some magic number...

    box.p.log <- matrix(NA, ncol = idx_a0_n, nrow = nrow(sim.split.idx))

    ### Loop 3) (idx_a0) Repetitions per a0 (i.e. number of compatibility scores computed per exposure time interval)
    for (idx_a0 in 1:idx_a0_n) {
      lambda <- sample_coefs(sim.poisson.lambda, c(cov.names, interval.names))
      theta <- sample_coefs(sim.poisson.theta, interval.names)

      # debugging code - prevent theta from being positive
      for (i in 1:length(theta)) {
        if (theta[i] > 0) theta[i] <- min(theta)
      }

      ### Loop 3) (idx_a0) generate y.h.pred incorporating variability in estimating lambda and assess compatibility, use interval + expo
      rate_t <- exp(sim.mm %*% lambda) # this is the other lambda!
      rate_c <- exp(sim.mm[, interval.names] %*% theta)

      a <- log(sim.split.idx$expo)
      boxp_fun <- switch(control_pars$boxp_dist,
                         "W" = get_boxp_W,
                         "density" = get_boxp_density,
                         "kde" = get_boxp_kde
      )

      box.p.log[, idx_a0] <- boxp_fun(a, rate_t, rate_c)

    } # ends for (idx_a0 in 1:idx_a0_n)

    ### Loop 2) (idx) Compute case-specific weights
    a0 <- rowMeans(box.p.log)
    a0[a0 == 0] <- 1E-6 # a0 should never be exactly 0
    a0
  }
  replicate(control_pars$samples_1, iteration_fun(sim.split.ex, sim.mm))

}


