# Define a function for analysing one simulated dataset
sim_single_matrix <- function(n = 600,
                              hr_or = 0.6,
                              drift_hr_or = 1.5,
                              cov1_hr_or = 1.2,
                              cov2_hr_or = 0.8) {
  # Depends
  require(simsurv)
  require(broom)
  require(survival)

  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(
    id = 1:n,
    trt = rbinom(n, 1, 0.5),
    cov1 = rbinom(n, 1, 0.8),
    cov2 = rnorm(n, 0, 1)
  )
  cov$ext <- ifelse(cov$trt == 1L, 0L, rbinom(sum(cov$trt), 1, 0.5))

  # Simulate the event times
  dat <- simsurv(
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(
      trt = log(hr_or),
      ext = log(drift_hr_or),
      cov1 = log(cov1_hr_or),
      cov2 = log(cov2_hr_or)
    ),
    x = cov,
    maxt = 5
  )

  dat$censor <- 1 - dat$status

  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)

  # Add in binary endpoint
  lp <- dat$trt * log(hr_or) + dat$ext * log(drift_hr_or) + dat$cov1 * log(cov1_hr_or) + dat$cov2 * log(cov2_hr_or)
  exlp <- 1 / (1 + exp(-lp))

  dat$bin_endpoint <- rbinom(n, 1, exlp)

  as.matrix(dat)
}

sim_list_of_matrices <- function(hr_ors,
                                 drift_hr_ors,
                                 cov1_hr_ors,
                                 cov2_hr_ors,
                                 iter,
                                 n) {
  combos <- expand.grid(
    hr_or = hr_ors,
    drift_hr_or = drift_hr_ors,
    cov1_hr_or = cov1_hr_ors,
    cov2_hr_or = cov2_hr_ors
  )

  mat_list <- vector("list", NROW(combos))
  for (i in seq_along(mat_list)) {
    mat_list[[i]] <- vector("list", iter)
  }
  counter <- 1
  total <- NROW(combos) * iter
  st <- Sys.time()
  for (i in seq_along(mat_list)) {
    for (j in 1:iter) {
      mat_list[[i]][[j]] <- sim_single_matrix(
        n = n,
        hr_or = combos$hr_or[[i]],
        drift_hr_or = combos$drift_hr_or[[i]],
        cov1_hr_or = combos$cov1_hr_or[[i]],
        cov2_hr_or = combos$cov2_hr_or[[i]]
      )
      en <- Sys.time()
      sten <- as.numeric(difftime(en, st, units = "mins"))
      speed <- counter / sten
      remain <- (total - counter) / speed
      cat(
        "\r", "Made ", counter, " of ", total, " -- ",
        format(round(100 * counter / total, 2), nsmall = 2),
        "%", " (", format(round(remain, 1), nsmall = 1), " minutes left ) ",
        "|", rep("-", floor(50 * counter / total)), rep(" ", 50 - floor(50 * counter / total)), "|",
        sep = ""
      )
      counter <- counter + 1
    }
  }

  list(
    mat_list = mat_list,
    guide = combos
  )
}

ml1 <- sim_list_of_matrices(
  seq(0.6, 1.2, 0.2),
  seq(0.8, 1.2, 0.4),
  seq(0.6, 1.2, 0.4),
  seq(0.6, 1.2, 0.4),
  20,
  300
)
