# Error checking----
test_that("mcmc_sample.default() default method throws error", {
  df_wrong_input <- data.frame(a = 2:4, b = 3:5)
  expect_error(
    mcmc_sample(df_wrong_input),
    "Objects of class .data.frame. not supported by .mcmc_sample()."
  )
  expect_error(
    mcmc_sample(tibble::as_tibble(df_wrong_input)),
    "Objects of class .tbl_df.*.data.frame. not supported by .mcmc_sample()."
  )
})

test_that("mcmc_sample handles Analysis objects not ready to sample", {
  object <- psborrow2:::.analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(c("cov1", "cov2"), normal_prior(0, 1000)),
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      normal_prior(0, 1000)
    ),
    borrowing = borrowing_details(
      "Full borrowing",
      ext_flag_col = "ext"
    )
  )
  expect_error(mcmc_sample(object), "Cannot sample object.")
})


# Exponential models, no BDB ----
test_that("mcmc_sample for Analysis works for full borrowing, exponential dist", {
  skip_on_cran()
  skip_on_ci()
  full_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))[["trt"]])

  full_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes <- mcmc_sample(full_exp_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_exp_bayes, "CmdStanMCMC")
  expect_equal(full_exp_bayes$summary("HR_trt", "median")[[2]], full_exp, tolerance = 0.05)
})

test_that("mcmc_sample for Analysis works for no borrowing, exponential dist", {
  skip_on_cran()
  skip_on_ci()
  no_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))[["trt"]])

  no_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes <- mcmc_sample(no_exp_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_exp_bayes, "CmdStanMCMC")
  expect_equal(
    no_exp_bayes$summary("HR_trt", "median")[[2]],
    no_exp,
    tolerance = .05
  )
})


test_that("mcmc_sample for Analysis works for full borrowing, exponential dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))[["trt"]])

  full_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes_c1 <- mcmc_sample(full_exp_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_exp_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_exp_bayes_c1$summary("HR_trt", "median")[[2]],
    full_exp_c1,
    tolerance = .05
  )
})


test_that("mcmc_sample for Analysis works for no borrowing, exponential dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))[["trt"]])

  no_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes_c1 <- mcmc_sample(no_exp_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_exp_bayes_c1, "CmdStanMCMC")
  expect_equal(
    no_exp_bayes_c1$summary("HR_trt", "median")[[2]],
    no_exp_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for full borrowing, exponential dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_exp_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))[["trt"]])

  full_exp_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes_c2 <- mcmc_sample(full_exp_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_exp_bayes_c2, "CmdStanMCMC")
  expect_equal(
    full_exp_bayes_c2$summary("HR_trt", "median")[[2]],
    full_exp_c2,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, exponential dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_exp_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))[["trt"]])

  no_exp_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = exp_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes_c2 <- mcmc_sample(no_exp_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_exp_bayes_c2, "CmdStanMCMC")
  expect_equal(
    no_exp_bayes_c2$summary("HR_trt", "median")[[2]],
    no_exp_c2,
    tolerance = .05
  )
})

# Weibull models, no BDB ----
test_that("mcmc_sample for Analysis works for full borrowing, Weibull dist", {
  skip_on_cran()
  skip_on_ci()
  full_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = "weibullPH"
  ))[["trt"]])

  full_weib_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes <- mcmc_sample(full_weib_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_weib_bayes, "CmdStanMCMC")
  expect_equal(
    full_weib_bayes$summary("HR_trt", "median")[[2]],
    full_weib,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, Weibull dist", {
  skip_on_cran()
  skip_on_ci()
  no_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "weibullPH"
  ))[["trt"]])

  no_weib_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes <- mcmc_sample(no_weib_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_weib_bayes, "CmdStanMCMC")
  expect_equal(
    no_weib_bayes$summary("HR_trt", "median")[[2]],
    no_weib,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for full borrowing, weibull dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_weib_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "weibullPH"
  ))[["trt"]])

  full_weib_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes_c1 <- mcmc_sample(full_weib_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_weib_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_weib_bayes_c1$summary("HR_trt", "median")[[2]],
    full_weib_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, Weibull dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_weib_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "WeibullPH"
  ))[["trt"]])

  no_weib_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes_c1 <- mcmc_sample(no_weib_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_weib_bayes_c1, "CmdStanMCMC")
  expect_equal(
    no_weib_bayes_c1$summary("HR_trt", "median")[[2]],
    no_weib_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for full borrowing, Weibull dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_weib_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    dist = "WeibullPH"
  ))[["trt"]])

  full_weib_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes_c2 <- mcmc_sample(full_weib_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_weib_bayes_c2, "CmdStanMCMC")
  expect_equal(
    full_weib_bayes_c2$summary("HR_trt", "median")[[2]],
    full_weib_c2,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, Weibull dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_weib_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "WeibullPH"
  ))[["trt"]])

  no_weib_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = weib_ph_surv_dist(
      "time",
      "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes_c2 <- mcmc_sample(no_weib_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_weib_bayes_c2, "CmdStanMCMC")
  expect_equal(
    no_weib_bayes_c2$summary("HR_trt", "median")[[2]],
    no_weib_c2,
    tolerance = .05
  )
})

# Logistic regression models, no BDB ----
test_that("mcmc_sample for Analysis works for full borrowing, binomial dist", {
  skip_on_cran()
  skip_on_ci()
  full_bin <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))[["trt"]])

  full_bin_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes <- mcmc_sample(full_bin_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_bin_bayes, "CmdStanMCMC")
  expect_equal(
    full_bin_bayes$summary("OR_trt", "median")[[2]],
    full_bin,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, binomial dist", {
  skip_on_cran()
  skip_on_ci()
  no_bin <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))[["trt"]])

  no_bin_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes <- mcmc_sample(no_bin_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_bin_bayes, "CmdStanMCMC")
  expect_equal(
    no_bin_bayes$summary("OR_trt", "median")[[2]],
    no_bin,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for full borrowing, binomial dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_bin_c1 <- exp(coef(glm(
    resp ~ trt + cov1,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))[["trt"]])

  full_bin_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes_c1 <- mcmc_sample(full_bin_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_bin_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_bin_bayes_c1$summary("OR_trt", "median")[[2]],
    full_bin_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, binomial dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_bin_c1 <- exp(coef(glm(
    resp ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))[["trt"]])

  no_bin_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes_c1 <- mcmc_sample(no_bin_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_bin_bayes_c1, "CmdStanMCMC")
  expect_equal(
    no_bin_bayes_c1$summary("OR_trt", "median")[[2]],
    no_bin_c1,
    tolerance = 0.05
  )
})

test_that("mcmc_sample for Analysis works for full borrowing, binomial dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_bin_c2 <- exp(coef(glm(
    resp ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))[["trt"]])

  full_bin_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes_c2 <- mcmc_sample(full_bin_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(full_bin_bayes_c2, "CmdStanMCMC")
  expect_equal(
    full_bin_bayes_c2$summary("OR_trt", "median")[[2]],
    full_bin_c2,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for no borrowing, binomial dist, two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_bin_c2 <- exp(coef(glm(
    resp ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))[["trt"]])

  no_bin_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes_c2 <- mcmc_sample(no_bin_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )
  expect_r6(no_bin_bayes_c2, "CmdStanMCMC")
  expect_equal(
    no_bin_bayes_c2$summary("OR_trt", "median")[[2]],
    no_bin_c2,
    tolerance = 0.05
  )
})

# Exponential models, BDB conservative----
test_that("mcmc_sample for Analysis works for exponential BDB, conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  exp_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    exp_bdb_conservative,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("HR_trt")
  expect_equal(result_summary[["median"]], 0.85, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.62, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.18, tolerance = .05)
})

# Exponential models, BDB aggressive----
test_that("mcmc_sample for Analysis works for exponential BDB, aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  exp_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    exp_bdb_aggressive,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("HR_trt")
  expect_equal(result_summary[["median"]], 0.80, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.58, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.11, tolerance = .05)
})

# Weibull models, BDB conservative----
test_that("mcmc_sample for Analysis works for Weibull BDB, conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  weib_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 100000),
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    weib_bdb_conservative,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("HR_trt")
  expect_equal(result_summary[["median"]], 0.87, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.63, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.21, tolerance = .05)
})

# Weibull models, BDB aggressive----
test_that("mcmc_sample for Analysis works for Weibull BDB, aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  weib_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      shape_prior = normal_prior(0, 100000),
      baseline_prior = normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    weib_bdb_aggressive,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("HR_trt")
  expect_equal(result_summary[["median"]], 0.80, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.57, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.12, tolerance = .05)
})


# Logistic regression models, BDB conservative----
test_that("mcmc_sample for Analysis works for logistic regression BDB, conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  bin_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    bin_bdb_conservative,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("OR_trt")
  expect_equal(result_summary[["median"]], 1.19, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.63, tolerance = .05)
  expect_equal(result_summary[["q95"]], 2.08, tolerance = .05)
})

# Logistic regression models, BDB aggressive----
test_that("mcmc_sample for Analysis works for logistic regression BDB, aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  bin_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp", normal_prior(0, 100000)),
    borrowing = borrowing_details("BDB",
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )
  result <- mcmc_sample(
    bin_bdb_aggressive,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  result_summary <- result$summary("OR_trt")
  expect_equal(result_summary[["median"]], 1.65, tolerance = .05)
  expect_equal(result_summary[["q5"]], 1.11, tolerance = .05)
  expect_equal(result_summary[["q95"]], 2.44, tolerance = .05)
})