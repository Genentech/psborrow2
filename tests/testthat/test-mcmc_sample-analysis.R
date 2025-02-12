test_that("mcmc_sample gracefully fails if cmdstanr is unavailable", {
  skip_if(is_cmdstanr_available())

  expect_warning(
    object <- create_analysis_obj(
      data_matrix = example_matrix,
      outcome = outcome_surv_exponential(
        time_var = "time",
        cens_var = "cnsr",
        prior_normal(0, 100000)
      ),
      borrowing = borrowing_hierarchical_commensurate(
        ext_flag_col = "ext",
        tau_prior = prior_gamma(0.001, 0.001)
      ),
      treatment = treatment_details("trt", prior_normal(0, 100000))
    )
  )

  expect_error(
    result <- mcmc_sample(
      object,
      iter_warmup = 2000,
      iter_sampling = 2000,
      chains = 1
    ),
    regexp = "Cannot sample object",
    fixed = TRUE
  )
})

skip_if_not(check_cmdstan())

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
    covariates = add_covariates(c("cov1", "cov2"), prior_normal(0, 1000)),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
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
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_exp_bayes <- mcmc_sample(full_exp_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_exp_bayes <- mcmc_sample(no_exp_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    covariates = add_covariates("cov1", prior_normal(0, 100000)),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_exp_bayes_c1 <- mcmc_sample(full_exp_bayes_c1_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(full_exp_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_exp_bayes_c1$summary("HR_trt", "median")[[2]],
    full_exp_c1,
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
      prior_normal(0, 100000)
    ),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_exp_bayes_c2 <- mcmc_sample(no_exp_bayes_c2_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 100000),
      prior_normal(0, 100000)
    ),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_weib_bayes <- mcmc_sample(full_weib_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 100000),
      prior_normal(0, 100000)
    ),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_weib_bayes <- mcmc_sample(no_weib_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    covariates = add_covariates("cov1", prior_normal(0, 100000)),
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 100000),
      prior_normal(0, 100000)
    ),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_weib_bayes_c1 <- mcmc_sample(full_weib_bayes_c1_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(full_weib_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_weib_bayes_c1$summary("HR_trt", "median")[[2]],
    full_weib_c1,
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
      prior_normal(0, 100000)
    ),
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 100000),
      prior_normal(0, 100000)
    ),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_weib_bayes_c2 <- mcmc_sample(no_weib_bayes_c2_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_bin_bayes <- mcmc_sample(full_bin_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_bin_bayes <- mcmc_sample(no_bin_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
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
    covariates = add_covariates("cov1", prior_normal(0, 100000)),
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  full_bin_bayes_c1 <- mcmc_sample(full_bin_bayes_c1_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(full_bin_bayes_c1, "CmdStanMCMC")
  expect_equal(
    full_bin_bayes_c1$summary("OR_trt", "median")[[2]],
    full_bin_c1,
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
      prior_normal(0, 100000)
    ),
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  no_bin_bayes_c2 <- mcmc_sample(no_bin_bayes_c2_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(no_bin_bayes_c2, "CmdStanMCMC")
  expect_equal(
    no_bin_bayes_c2$summary("OR_trt", "median")[[2]],
    no_bin_c2,
    tolerance = 0.05
  )
})

# Continuous normal models, Full Borrowing ----
test_that("mcmc_sample for Analysis works for normal with full borrowing", {
  skip_on_cran()
  skip_on_ci()
  set.seed(123)

  outcome_col <- 5 + example_matrix[, "trt"] + example_matrix[, "cov1"] + 2 * example_matrix[, "cov2"] +
    0.5 * example_matrix[, "cov3"] - 1.5 * example_matrix[, "cov4"] + rnorm(500, 0, 1)

  outcome <- outcome_cont_normal(
    continuous_var = "outcome",
    baseline_prior = prior_normal(0, 100),
    std_dev_prior=prior_half_cauchy(1, 5)
  )
  borrowing <- borrowing_full(
    ext_flag_col = "ext"
  )
  treatment <- treatment_details(
    trt_flag_col = "trt",
    trt_prior = prior_normal(0, 1000)
  )
  anls_obj <- create_analysis_obj(
    data_matrix = cbind(example_matrix, outcome = outcome_col),
    outcome =  outcome,
    borrowing = borrowing,
    treatment = treatment,
    quiet = FALSE
  )
  result <- mcmc_sample(
    anls_obj,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )

  result_summary <- result$summary(c("alpha", "beta_trt"))
  expect_equal(result_summary[["median"]], c(6.58, 0.42), tolerance = .05)
  expect_equal(result_summary[["q5"]], c(6.45, 0.128), tolerance = .05)
  expect_equal(result_summary[["q95"]], c(6.72, 0.72), tolerance = .05)
})

# Continuous normal models, BDB ----
test_that("mcmc_sample for Analysis works for normal with BDB", {
  skip_on_cran()
  skip_on_ci()
  set.seed(123)

  outcome_col <- 5 + example_matrix[, "trt"] + example_matrix[, "cov1"] + 2 * example_matrix[, "cov2"] +
    0.5 * example_matrix[, "cov3"] - 1.5 * example_matrix[, "cov4"] + rnorm(500, 0, 1)

  outcome <- outcome_cont_normal(
    continuous_var = "outcome",
    baseline_prior = prior_normal(0, 100),
    std_dev_prior=prior_half_cauchy(1, 5)
  )
  borrowing <- borrowing_hierarchical_commensurate(
    ext_flag_col = "ext",
    tau_prior = prior_gamma(0.001, 0.001)
  )
  treatment <- treatment_details(
    trt_flag_col = "trt",
    trt_prior = prior_normal(0, 1000)
  )
  anls_obj <- create_analysis_obj(
    data_matrix = cbind(example_matrix, outcome = outcome_col),
    outcome =  outcome,
    borrowing = borrowing,
    treatment = treatment,
    quiet = FALSE
  )
  result <- mcmc_sample(
    anls_obj,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )

  result_summary <- result$summary("beta_trt")
  expect_equal(result_summary[["median"]], 1.20, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.715, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.71, tolerance = .05)
})

# Weibull models, BDB aggressive----
test_that("mcmc_sample for Analysis works for Weibull BDB, aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  weib_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_weibull_ph(
      time_var = "time",
      cens_var = "cnsr",
      shape_prior = prior_normal(0, 100000),
      baseline_prior = prior_normal(0, 100000)
    ),
    borrowing = borrowing_hierarchical_commensurate(
      ext_flag_col = "ext",
      tau_prior = prior_gamma(1, 0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )
  result <- mcmc_sample(
    weib_bdb_aggressive,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )

  result_summary <- result$summary("HR_trt")
  expect_equal(result_summary[["median"]], 0.80, tolerance = .05)
  expect_equal(result_summary[["q5"]], 0.57, tolerance = .05)
  expect_equal(result_summary[["q95"]], 1.12, tolerance = .05)
})

# Logistic regression models, BDB aggressive----
test_that("mcmc_sample for Analysis works for logistic regression BDB, aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  bin_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_hierarchical_commensurate(
      ext_flag_col = "ext",
      tau_prior = prior_gamma(1, 0.001)
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )
  result <- mcmc_sample(
    bin_bdb_aggressive,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )

  result_summary <- result$summary("OR_trt")
  expect_equal(result_summary[["median"]], 1.65, tolerance = .05)
  expect_equal(result_summary[["q5"]], 1.10, tolerance = .05)
  expect_equal(result_summary[["q95"]], 2.44, tolerance = .05)
})


# Piecewise exponential, no BDB ----
test_that("mcmc_sample for Analysis works for full borrowing, piecewise exponential dist", {
  skip_on_cran()
  skip_on_ci()
  library(eha)
  cuts = c(1, 5, 10)
  pem_eha <- eha::pchreg(survival::Surv(time, status) ~ trt + cov1 + cov2, data = as.data.frame(psborrow2::example_matrix), cuts = c(0, cuts, 1000))

  full_pem_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_full("ext"),
    treatment = treatment_details("trt", prior_normal(0, 100000)),
    covariates = add_covariates(c("cov1", "cov2"), prior_normal(0, 100000))
  )

  full_pem_bayes <- mcmc_sample(full_pem_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  expect_r6(full_pem_bayes, "CmdStanMCMC")
  expect_equal(full_pem_bayes$summary("beta_trt")[[2]], pem_eha$coefficients[['trt']], tolerance = 0.05)
  expect_equal(full_pem_bayes$summary("beta[1]")[[2]], pem_eha$coefficients[['cov1']], tolerance = 0.05)
  expect_equal(full_pem_bayes$summary("beta[2]")[[2]], pem_eha$coefficients[['cov2']], tolerance = 0.05)

  # Check that the cut points are the same
  expect_equal(full_pem_bayes$summary("alpha[1]")[[2]], log(pem_eha$hazards[1]), tolerance = 0.05)
  expect_equal(full_pem_bayes$summary("alpha[2]")[[2]], log(pem_eha$hazards[2]), tolerance = 0.05)
  expect_equal(full_pem_bayes$summary("alpha[3]")[[2]], log(pem_eha$hazards[3]), tolerance = 0.05)
  expect_equal(full_pem_bayes$summary("alpha[4]")[[2]], log(pem_eha$hazards[4]), tolerance = 0.05)

})

# Piecewise exponential, BDB ----
test_that("mcmc_sample for Analysis works for BDB, piecewise exponential dist", {
  skip_on_cran()
  skip_on_ci()
  cuts = c(1, 5, 10)

  # Make commensurate matrix
  internal_as_external <- example_matrix[example_matrix[, 'ext'] == 0 & example_matrix[,'trt'] == 0,]
  internal_as_external[, 'ext'] <- 1
  internal_as_external[, 'id'] <- seq(10000, 10000 + nrow(internal_as_external) - 1)
  commensurate_matrix <- rbind(
    example_matrix[example_matrix[,'ext'] == 0,],
    internal_as_external
  )

  ## Conservative commensurate
  bdb_pem_bayes_commens_conserv_ao <- create_analysis_obj(
    data_matrix = commensurate_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_gamma(0.001, 0.001)),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  bdb_pem_bayes_commens_conserv <- mcmc_sample(bdb_pem_bayes_commens_conserv_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  tau_commens_conserv <- bdb_pem_bayes_commens_conserv$summary("tau")[["median"]]

  ## Conservative incommensurate
  bdb_pem_bayes_incommens_conserv_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_gamma(0.001, 0.001)),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  bdb_pem_bayes_incommens_conserv <- mcmc_sample(bdb_pem_bayes_incommens_conserv_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  tau_incommens_conserv <- bdb_pem_bayes_incommens_conserv$summary("tau")[["median"]]

  ## Aggressive commensurate
  bdb_pem_bayes_commens_aggr_ao <- create_analysis_obj(
    data_matrix = commensurate_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_gamma(1, .001)),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  bdb_pem_bayes_commens_aggr <- mcmc_sample(bdb_pem_bayes_commens_aggr_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  tau_commens_aggr <- bdb_pem_bayes_commens_aggr$summary("tau")[["median"]]

  ## Aggressive incommensurate
  bdb_pem_bayes_incommens_aggr_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_hierarchical_commensurate("ext", prior_gamma(1, .001)),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  bdb_pem_bayes_incommens_aggr <- mcmc_sample(bdb_pem_bayes_incommens_aggr_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  tau_incommens_aggr <- bdb_pem_bayes_incommens_aggr$summary("tau")[["median"]]

  # Comparisons
  expect_true(tau_commens_conserv > tau_incommens_conserv)
  expect_true(tau_commens_aggr > tau_incommens_aggr)
  expect_true(tau_commens_aggr > tau_commens_conserv)
  expect_true(tau_incommens_aggr > tau_incommens_conserv)

})

# Fixed Power Prior ------
test_that("mcmc_sample for Analysis works for fixed power prior borrowing, exponential dist", {
  skip_on_cran()
  skip_on_ci()
  power <- 1 - 0.3 * example_matrix[, "ext"]

  exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "exponential",
    weights = power
  ))[["trt"]])


  fpp_exp_bayes_ao <- create_analysis_obj(
    data_matrix = cbind(example_matrix, power = power),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_fixed_power_prior(
      ext_flag_col = "ext",
      power_col = "power"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  fpp_exp_bayes <- mcmc_sample(
    fpp_exp_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(fpp_exp_bayes, "CmdStanMCMC")
  expect_equal(
    fpp_exp_bayes$summary("HR_trt", "median")[[2]],
    exp_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for fixed power prior, exponential dist, one covariate", {
  skip_on_cran()
  skip_on_ci()
  power <- 1 - 0.3 * example_matrix[, "ext"]
  fpp_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "exponential",
    weights = power
  ))[["trt"]])

  fpp_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = cbind(example_matrix, power = power),
    covariates = add_covariates("cov1", prior_normal(0, 100000)),
    outcome = outcome_surv_exponential("time", "cnsr", prior_normal(0, 100000)),
    borrowing = borrowing_fixed_power_prior(
      ext_flag_col = "ext",
      power_col = "power"
    ),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  fpp_exp_bayes_c1 <- mcmc_sample(
    fpp_exp_bayes_c1_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(fpp_exp_bayes_c1, "CmdStanMCMC")
  expect_equal(
    fpp_exp_bayes_c1$summary("HR_trt", "median")[[2]],
    fpp_exp_c1,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for fixed power prior borrowing, normal dist", {
  set.seed(123)
  power <- 1 - 0.5 * example_matrix[, "ext"]
  outcome_col <- 5 + example_matrix[, "trt"] + example_matrix[, "cov1"] + 2 * example_matrix[, "cov2"] +
    0.5 * example_matrix[, "cov3"] - 1.5 * example_matrix[, "cov4"] + rnorm(500, 0, 1)

  outcome <- outcome_cont_normal(
    continuous_var = "outcome",
    baseline_prior = prior_normal(0, 100),
    std_dev_prior = prior_half_cauchy(1, 5)
  )
  borrowing <- borrowing_fixed_power_prior(
    ext_flag_col = "ext",
    power_col = "power"
  )
  treatment <- treatment_details(
    trt_flag_col = "trt",
    trt_prior = prior_normal(0, 1000)
  )
  anls_obj <- create_analysis_obj(
    data_matrix = cbind(example_matrix, outcome = outcome_col, power = power),
    outcome = outcome,
    borrowing = borrowing,
    treatment = treatment,
    quiet = FALSE
  )
  result <- mcmc_sample(
    anls_obj,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )

  result_summary <- result$summary(c("alpha", "beta_trt"))
  expect_equal(result_summary[["median"]], c(6.44, 0.72), tolerance = .05)
  expect_equal(result_summary[["q5"]], c(6.25, 0.38), tolerance = .05)
  expect_equal(result_summary[["q95"]], c(6.62, 1.06), tolerance = .05)
})


test_that("mcmc_sample for Analysis works for fixed power prior borrowing, logistic dist", {
  skip_on_cran()
  skip_on_ci()
  set.seed(123)
  power <- 1 - 0.5 * example_matrix[, "ext"]
  fpp_bin_bayes_ao <- create_analysis_obj(
    data_matrix = cbind(example_matrix, power = power),
    outcome = outcome_bin_logistic("resp", prior_normal(0, 100000)),
    borrowing = borrowing_fixed_power_prior("ext", power_col = "power"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  fpp_bin_bayes <- mcmc_sample(
    fpp_bin_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(fpp_bin_bayes, "CmdStanMCMC")
  expect_equal(
    fpp_bin_bayes$summary("OR_trt", "median")[[2]],
    1.59,
    tolerance = .05
  )
})

test_that("mcmc_sample for Analysis works for fixed power prior borrowing, PEM dist", {
  skip_on_cran()
  skip_on_ci()
  cuts <- c(1, 5, 10)
  set.seed(1234)
  internal_as_external <- example_matrix[example_matrix[, "ext"] == 0 & example_matrix[, "trt"] == 0, ]
  internal_as_external[, "ext"] <- 1
  internal_as_external[, "id"] <- seq(10000, 10000 + nrow(internal_as_external) - 1)
  data_matrix <- rbind(
    example_matrix[example_matrix[, "ext"] == 0, ],
    internal_as_external
  )
  power <- 1 - 0.5 * data_matrix[, "ext"]
  data_matrix <- cbind(data_matrix, power = power)

  ## Conservative commensurate
  fpp_pem_ao <- create_analysis_obj(
    data_matrix = data_matrix,
    outcome = outcome_surv_pem("time", "cnsr", prior_normal(0, 100000), cut_points = cuts),
    borrowing = borrowing_fixed_power_prior("ext", "power"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  fpp_pem <- mcmc_sample(
    fpp_pem_ao,
    iter_warmup = 2000,
    iter_sampling = 5000,
    chains = 2
  )
  beta_trt <- fpp_pem$summary("beta_trt")[["median"]]
  expect_equal(beta_trt, -0.11, tolerance = 0.05)
})

test_that("mcmc_sample for Analysis works for fixed power prior borrowing, Weibull dist", {
  skip_on_cran()
  skip_on_ci()

  data_matrix <- cbind(example_matrix, power = 1 - 0.5 * example_matrix[, "ext"])

  fpp_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(data_matrix),
    dist = "weibullPH",
    weights = power
  ))[["trt"]])

  fpp_weib_bayes_ao <- create_analysis_obj(
    data_matrix = data_matrix,
    outcome = outcome_surv_weibull_ph(
      "time",
      "cnsr",
      prior_normal(0, 100000),
      prior_normal(0, 100000)
    ),
    borrowing = borrowing_fixed_power_prior("ext", "power"),
    treatment = treatment_details("trt", prior_normal(0, 100000))
  )

  fpp_weib_bayes <- mcmc_sample(
    fpp_weib_bayes_ao,
    iter_warmup = 2000,
    iter_sampling = 2000,
    chains = 1
  )
  expect_r6(fpp_weib_bayes, "CmdStanMCMC")
  expect_equal(
    fpp_weib_bayes$summary("HR_trt", "median")[[2]],
    fpp_weib,
    tolerance = .05
  )
})
