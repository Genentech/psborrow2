make_odds <- function(prob) {
  prob / (1 - prob)
}

make_prob <- function(odds) {
  odds / (1 + odds)
}

sim_single_matrix <- function(n = 500,
                              prob = c(0.1, 0.2, 0.7),
                              hr = 0.70,
                              or = 1.20,
                              inherent_drift_hr = 1.0,
                              inherent_drift_or = 1.0,
                              cov1_baseline_prob = 0.5,
                              cov2_baseline_prob = 0.25,
                              cov3_baseline_prob = 0.75,
                              cov4_baseline_prob = 0.5,
                              cov1_hr = 2.5,
                              cov2_hr = 2.5,
                              cov3_hr = 0.25,
                              cov4_hr = 0.25,
                              cov1_or = 0.5,
                              cov2_or = 0.5,
                              cov3_or = 1.50,
                              cov4_or = 1.50,
                              ext_or_cov1 = 3,
                              ext_or_cov2 = 3,
                              ext_or_cov3 = 0.25,
                              ext_or_cov4 = 0.25,
                              trt_or_cov1 = 1.5,
                              trt_or_cov2 = 1.5,
                              trt_or_cov3 = 0.75,
                              trt_or_cov4 = 0.75) {
  #' Creates simulated data for survival and binary endpoints w/ 4 covariates
  #'
  #' @param n number of simulated patients
  #' @param prob numeric vector of probabilities for internal control,
  #' external control, internal experimental, in that order.
  #' @param hr true HR
  #' @param or true OR
  #' @param inherent_drift_hr the baseline HR between internal
  #' and external controls not caused by covariates
  #' @param inherent_drift_or the baseline OR between internal
  #' and external controls not caused by covariates
  #' @param cov1_baseline_prob probability of cov1 in ext = 0 groups
  #' @param cov2_baseline_prob probability of cov2 in ext = 0 groups
  #' @param cov3_baseline_prob probability of cov3 in ext = 0 groups
  #' @param cov4_baseline_prob probability of cov4 in ext = 0 groups
  #' @param cov1_hr true HR for cov1
  #' @param cov2_hr true HR for cov2
  #' @param cov3_hr true HR for cov3
  #' @param cov4_hr true HR for cov4
  #' @param cov1_or true OR for cov1
  #' @param cov2_or true OR for cov2
  #' @param cov3_or true OR for cov3
  #' @param cov4_or true OR for cov4
  #' @param ext_or_cov1 OR of being cov1 for external controls
  #' @param ext_or_cov2 OR of being cov2 for external controls
  #' @param ext_or_cov3 OR of being cov3 for external controls
  #' @param ext_or_cov4 OR of being cov4 for external controls

  # checks
  if (sum(prob) != 1.0) {
    stop("prob must sum to 1")
  }

  # Depend
  require(simsurv)

  # Create a data frame with the subject IDs and treatment group
  cov <- data.frame(
    id = 1:n,
    ext = c(
      rep(0L, n * (prob[1] + prob[2])),
      rep(1L, n * prob[3])
    ),
    trt = c(
      rep(0L, n * prob[1]),
      rep(1L, n * prob[2]),
      rep(0L, n * prob[3])
    )
  )
  cov$cov1 <- cov$cov2 <- cov$cov3 <- cov$cov4 <- integer(length = nrow(cov))
  cov$cov1 <- cov$cov2 <- cov$cov3 <- cov$cov4 <- integer(length = nrow(cov))

  for (i in seq(1, NROW(cov))) {
    if (cov$ext[i] == 1) {
      cov$cov1[i] <- rbinom(1, 1, make_prob(ext_or_cov1 * make_odds(cov1_baseline_prob)))
      cov$cov2[i] <- rbinom(1, 1, make_prob(ext_or_cov2 * make_odds(cov2_baseline_prob)))
      cov$cov3[i] <- rbinom(1, 1, make_prob(ext_or_cov3 * make_odds(cov3_baseline_prob)))
      cov$cov4[i] <- rbinom(1, 1, make_prob(ext_or_cov4 * make_odds(cov4_baseline_prob)))
    } else if (cov$trt[i] == 1) {
      cov$cov1[i] <- rbinom(1, 1, make_prob(trt_or_cov1 * make_odds(cov1_baseline_prob)))
      cov$cov2[i] <- rbinom(1, 1, make_prob(trt_or_cov2 * make_odds(cov2_baseline_prob)))
      cov$cov3[i] <- rbinom(1, 1, make_prob(trt_or_cov3 * make_odds(cov3_baseline_prob)))
      cov$cov4[i] <- rbinom(1, 1, make_prob(trt_or_cov4 * make_odds(cov4_baseline_prob)))
    } else {
      cov$cov1[i] <- rbinom(1, 1, cov1_baseline_prob)
      cov$cov2[i] <- rbinom(1, 1, cov2_baseline_prob)
      cov$cov3[i] <- rbinom(1, 1, cov3_baseline_prob)
      cov$cov4[i] <- rbinom(1, 1, cov4_baseline_prob)
    }
  }

  # Simulate the event times
  dat <- simsurv(
    lambdas = 0.1,
    dist = "exponential",
    betas = c(
      trt = log(hr),
      ext = log(inherent_drift_hr),
      cov1 = log(cov1_hr),
      cov2 = log(cov2_hr),
      cov3 = log(cov3_hr),
      cov4 = log(cov4_hr)
    ),
    x = cov,
    maxt = 50
  )

  dat$censor <- 1 - dat$status

  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)

  # Add in binary endpoint
  lp <- dat$trt * log(or) + dat$ext * log(inherent_drift_or) +
    dat$cov1 * log(cov1_or) +
    dat$cov2 * log(cov2_or) +
    dat$cov3 * log(cov3_or) +
    dat$cov4 * log(cov4_or)

  exlp <- 1 / (1 + exp(-lp))

  dat$bin_endpoint <- rbinom(n, 1, exlp)

  dat <- dat[, c(
    "id", "ext", "trt", "cov4", "cov3", "cov2", "cov1",
    "eventtime", "status", "censor", "bin_endpoint"
  )]

  colnames(dat) <- c(
    "id", "ext", "trt", "cov4", "cov3", "cov2", "cov1",
    "time", "status", "cnsr", "resp"
  )

  as.matrix(dat)
}

set.seed(123)
example_matrix <- sim_single_matrix()

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(example_matrix, overwrite = TRUE)
}
