# Helper functions for case weight calculation


# Main function called from `prepare_stan_data_inputs` ----------
analysis_case_weights <- function(data_in, analysis_object) {

  df <- as.data.frame(analysis_object@data_matrix)
  models <- case_weight_models(
    df,
    analysis_object@covariates@covariates
  )

  a_n <- calculate_case_weights(
    models$sim.poisson,
    models$sim.poisson.lambda,
    models$sim.poisson.theta,
    ext_data = df[df$ext == 1, ],
    diff = c(diff(c(0, analysis_object@outcome@cut_points)), Inf),
    pars = control_pars(
      "W", analysis_object@borrowing@samples[1], analysis_object@borrowing@samples[2]
    )
  )
  a <- rowMeans(a_n)

  fp <- polynomial_transform(a, p = analysis_object@borrowing@p)
  gc <- global_transform(mean(a), q = analysis_object@borrowing@q, c = analysis_object@borrowing@c)

  data_in[["weights"]] <- rep(1, data_in$N)
  data_in[["weights"]][df$ext == 1] <- fp * gc

  data_in
}


# Calculate the Poisson posterior models -------
case_weight_models <- function(data, covariates) {
  base_formula <- reformulate(covariates, response = "1 - cnsr")
  data$interval <- factor(data[["__period__"]])

  # Fit model for historical events
  formula2_no_trt <- update(base_formula, ~ . + interval + offset(log(time)) - 1)
  sim.poisson <- fit_poisson_glm(formula2_no_trt, data[data[["ext"]] == 1, ])

  # Fit model for RCT events
  formula2 <- update(base_formula, ~ . + interval + offset(log(time)) - 1)
  sim.poisson.lambda <- fit_poisson_glm(formula2, data[data[["ext"]] == 0, ])

  # Fit model for historical censoring
  sim.poisson.theta <- fit_poisson_glm(
    paste0("cnsr ~ interval + offset(log(time)) - 1"),
    data[data[["ext"]] == 1, ]
  )

  list(
    sim.poisson = sim.poisson,
    sim.poisson.lambda = sim.poisson.lambda,
    sim.poisson.theta = sim.poisson.theta
  )
}

#' Fit a poisson GLM
#' @noRd
fit_poisson_glm <- function(formula, data) {
  glm(
    formula,
    family = poisson(link = "log"),
    data = data
  )
}


#' Set control parameters for Case Weights
#'
#' @param boxp_dist Which method to use for Box P distribution calculation:
#' `"kde"`: Sampling and density with `ks::kde`
#' `"density"`: Sampling and density with `stats::density`
#' `"W"`: Solve using Lambert W function
#' @noRd
control_pars <- function(boxp_dist = "W", samples_1 = 100, samples_2 = 100) {
  list(
    boxp_dist = boxp_dist,
    samples_1 = samples_1,
    samples_2 = samples_2
  )
}


# Sample coefs from GLM
sample_coefs <- function(model, covs) {
  coef.model <- model$coefficients
  cov.index <- names(coef.model) %in% c(covs)
  MASS::mvrnorm(1, coef.model[cov.index], vcov(model)[cov.index, cov.index])
}

# Transformation functions -------
polynomial_transform <- function(x, p) {
  2^(p - 1) * sign(x - 0.5) * abs(x - 0.5)^p + 0.5
}
global_transform <- function(x, q, c) {
  1 / (1 + exp(-(q * (x - c))))
}


#' Cumulative Probability of X ~ log(Y), where Y ~ Exp(rate)
#'
#' @param x value to evaluate probability
#' @param rate lambda parameter
#'
#' @return probability values
#' @noRd
plog_exp <- function(x, rate) {
  1 - exp(-rate * exp(x))
}


#' Density of X ~ log(Y), where Y ~ Exp(rate)
#' @param x value to evaluate density
#' @param rate lambda parameter
#'
#' @return density values
#' @noRd
dlog_exp <- function(x, rate) {
  rate * exp(x - rate * exp(x))
  # consider log
  # exp(
  #   log(rate) + x - rate*exp(x)
  # )
}

# #' Log Piecewise Exponential distribution
# #' @noRd
# plog_pcexp <- function(x, rate) {
#   1 - exp(-rate * exp(x))
# }
#
# dlog_pcexp <- function(x, rate, time) {
#   rate * exp(x - rate * exp(x))
#   # consider log
#   # exp(
#   #   log(rate) + x - rate*exp(x)
#   # )
# }

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
                                   ext_data,
                                   diff,
                                   pars) {


  sim.mm <- model.matrix(sim.poisson) # model matrix with external == 1 and covariates

  interval.names <- names(sim.poisson.theta$coefficients)
  cov.names <- names(sim.poisson.lambda$coefficients) |> grep(pattern = "interval", invert = TRUE, value = TRUE)

  iteration_fun <- function(ext_data, sim.mm) {
    # one sample of lambda from model of outcomes in historical data
    lambda <- sample_coefs(sim.poisson, c(cov.names, interval.names))

    # one sample theta from historical censoring model
    theta <- sample_coefs(sim.poisson.theta, interval.names)

    # Add imputation for multiple intervals in historical controls
    sim.split.idx <- ext_data

    # Identify rows which are alive and uncensored at the end of the period:
    impute_these_id <- sim.split.idx[["cnsr"]] == 1 & sim.split.idx[["time"]] == diff[sim.split.idx[["__period__"]]]
    rate_t <- exp(sim.mm[impute_these_id,] %*% lambda)
    rate_c <- exp(sim.mm[impute_these_id, interval.names] %*% theta)

    temp.y <- rexp(rep(1, sum(impute_these_id)), rate_t + rate_c)
    sim.split.idx$expo[impute_these_id] <- sim.split.idx$expo[impute_these_id] + temp.y

    # subset to historical only
    idx_a0_n <- pars$samples_2

    box.p.log <- matrix(NA, ncol = idx_a0_n, nrow = nrow(sim.split.idx))

    ### Loop 3) (idx_a0) Repetitions per a0 (i.e. number of compatibility scores computed per exposure time interval)
    for (idx_a0 in 1:idx_a0_n) {
      lambda <- sample_coefs(sim.poisson.lambda, c(cov.names, interval.names))
      theta <- sample_coefs(sim.poisson.theta, interval.names)

      # debugging code - prevent theta from being positive
      # if (any(theta > 0)) {browser()}
      # for (i in 1:length(theta)) {
      #   if (theta[i] > 0) theta[i] <- min(theta)
      # }

      ### Loop 3) (idx_a0) generate y.h.pred incorporating variability in estimating lambda and assess compatibility, use interval + expo
      rate_t <- exp(sim.mm %*% lambda) # this is the other lambda!
      rate_c <- exp(sim.mm[, interval.names] %*% theta)

      a <- log(sim.split.idx$time)
      boxp_fun <- switch(pars$boxp_dist,
                         "W" = get_boxp_W,
                         "density" = get_boxp_density,
                         "kde" = get_boxp_kde
      )
      box.p.log[, idx_a0] <- boxp_fun(a, rate_t, rate_c)
    }

    a0 <- rowMeans(box.p.log)
    a0[a0 == 0] <- 1E-6 # a0 should never be exactly 0
    a0
  }
  replicate(pars$samples_1, iteration_fun(ext_data, sim.mm))
}


