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

# Exponential models, no BDB ----
test_that("mcmc_sample.Analysis() works for full borrowing, exponential dist", {
  skip_on_cran()
  skip_on_ci()
  full_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))["trt"])

  full_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes <- mcmc_sample(full_exp_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_exp_bayes$summary("HR_trt")[, "median"][[1]],
      full_exp,
      tol = .02
    )
  )
})

test_that("mcmc_sample.Analysis() works for no borrowing, exponential dist", {
  skip_on_cran()
  skip_on_ci()
  no_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))["trt"])

  no_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes <- mcmc_sample(no_exp_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_exp_bayes$summary("HR_trt")[, "median"][[1]],
      no_exp,
      tol = .02
    )
  )
})


test_that("mcmc_sample.Analysis() works for full borrowing, exponential dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))["trt"])

  full_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes_c1 <- mcmc_sample(full_exp_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_exp_bayes_c1$summary("HR_trt")[, "median"][[1]],
      full_exp_c1,
      tol = .02
    )
  )
})


test_that("mcmc_sample.Analysis() works for no borrowing, exponential dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))["trt"])

  no_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes_c1 <- mcmc_sample(no_exp_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_exp_bayes_c1$summary("HR_trt")[, "median"][[1]],
      no_exp_c1,
      tol = .02
    )
  )
})



test_that("mcmc_sample.Analysis() works for full borrowing, exponential dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_exp_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))["trt"])

  full_exp_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes_c2 <- mcmc_sample(full_exp_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_exp_bayes_c2$summary("HR_trt")[, "median"][[1]],
      full_exp_c2,
      tol = .02
    )
  )
})


test_that("mcmc_sample.Analysis() works for no borrowing, exponential dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_exp_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "exponential"
  ))["trt"])

  no_exp_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_exp_bayes_c2 <- mcmc_sample(no_exp_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_exp_bayes_c2$summary("HR_trt")[, "median"][[1]],
      no_exp_c2,
      tol = .02
    )
  )
})

# Weibull models, no BDB ----
custom_weibull_ph <- list(
  name = "weibullPH",
  pars = c("shape", "scale"), location = "scale",
  transforms = c(log, log),
  inv.transforms = c(exp, exp),
  inits = function(t) {
    c(1, 1)
  }
)

test_that("mcmc_sample.Analysis() works for full borrowing, Weibull dist", {
  skip_on_cran()
  skip_on_ci()
  full_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = "weibullPH"
  ))["trt"])

  full_weib_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes <- mcmc_sample(full_weib_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_weib_bayes$summary("HR_trt")[, "median"][[1]],
      full_weib,
      tol = .02
    )
  )
})

test_that("mcmc_sample.Analysis() works for no borrowing, Weibull dist", {
  skip_on_cran()
  skip_on_ci()
  no_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "weibullPH"
  ))["trt"])

  no_weib_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes <- mcmc_sample(no_weib_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_weib_bayes$summary("HR_trt")[, "median"][[1]],
      no_weib,
      tol = .02
    )
  )
})

test_that("mcmc_sample.Analysis() works for full borrowing, weibull dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_weib_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "weibullPH"
  ))["trt"])

  full_weib_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes_c1 <- mcmc_sample(full_weib_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_weib_bayes_c1$summary("HR_trt")[, "median"][[1]],
      full_weib_c1,
      tol = .02
    )
  )
})

test_that("mcmc_sample.Analysis() works for no borrowing, Weibull dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_weib_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "WeibullPH"
  ))["trt"])

  no_weib_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes_c1 <- mcmc_sample(no_weib_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_weib_bayes_c1$summary("HR_trt")[, "median"][[1]],
      no_weib_c1,
      tol = .02
    )
  )
})


test_that("mcmc_sample.Analysis() works for full borrowing, Weibull dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_weib_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    dist = "WeibullPH"
  ))["trt"])

  full_weib_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_weib_bayes_c2 <- mcmc_sample(full_weib_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_weib_bayes_c2$summary("HR_trt")[, "median"][[1]],
      full_weib_c2,
      tol = .02
    )
  )
})


test_that("mcmc_sample.Analysis() works for no borrowing, Weibull dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_weib_c2 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1 - cnsr) ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    dist = "WeibullPH"
  ))["trt"])

  no_weib_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = weib_ph_surv_dist("time", "cnsr", normal_prior(0, 100000)),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_weib_bayes_c2 <- mcmc_sample(no_weib_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_weib_bayes_c2$summary("HR_trt")[, "median"][[1]],
      no_weib_c2,
      tol = .02
    )
  )
})

# Logistic regression models, no BDB ----
test_that("mcmc_sample.Analysis() works for full borrowing, binomial dist", {
  skip_on_cran()
  skip_on_ci()
  full_bin <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))["trt"])

  full_bin_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes <- mcmc_sample(full_bin_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_bin_bayes$summary("OR_trt")[, "median"][[1]],
      full_bin,
      tol = .02
    )
  )
})

test_that("mcmc_sample.Analysis() works for no borrowing, binomial dist", {
  skip_on_cran()
  skip_on_ci()
  no_bin <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))["trt"])

  no_bin_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes <- mcmc_sample(no_bin_bayes_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_bin_bayes$summary("OR_trt")[, "median"][[1]],
      no_bin,
      tol = .15
    )
  )
})

test_that("mcmc_sample.Analysis() works for full borrowing, binomial dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  full_bin_c1 <- exp(coef(glm(
    resp ~ trt + cov1,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))["trt"])

  full_bin_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes_c1 <- mcmc_sample(full_bin_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_bin_bayes_c1$summary("OR_trt")[, "median"][[1]],
      full_bin_c1,
      tol = .15
    )
  )
})

test_that("mcmc_sample.Analysis() works for no borrowing, binomial dist,
          one covariate", {
  skip_on_cran()
  skip_on_ci()
  no_bin_c1 <- exp(coef(glm(
    resp ~ trt + cov1,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))["trt"])

  no_bin_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates("cov1", normal_prior(0, 100000)),
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes_c1 <- mcmc_sample(no_bin_bayes_c1_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_bin_bayes_c1$summary("OR_trt")[, "median"][[1]],
      no_bin_c1,
      tol = .15
    )
  )
})

test_that("mcmc_sample.Analysis() works for full borrowing, binomial dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  full_bin_c2 <- exp(coef(glm(
    resp ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))["trt"])

  full_bin_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("Full borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_bin_bayes_c2 <- mcmc_sample(full_bin_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(full_bin_bayes_c2$summary("OR_trt")[, "median"][[1]],
      full_bin_c2,
      tol = .15
    )
  )
})


test_that("mcmc_sample.Analysis() works for no borrowing, binomial dist,
          two covariates", {
  skip_on_cran()
  skip_on_ci()
  no_bin_c2 <- exp(coef(glm(
    resp ~ trt + cov1 + cov2,
    data = as.data.frame(example_matrix[example_matrix[, "ext"] == 0, ]),
    family = binomial(link = "logit")
  ))["trt"])

  no_bin_bayes_c2_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      normal_prior(0, 100000)
    ),
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("No borrowing",
      normal_prior(0, 100000),
      ext_flag_col = "ext"
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  no_bin_bayes_c2 <- mcmc_sample(no_bin_bayes_c2_ao,
    iter_warmup = 1000,
    iter_sampling = 10000,
    chains = 1
  )

  expect_true(
    dplyr::near(no_bin_bayes_c2$summary("OR_trt")[, "median"][[1]],
      no_bin_c2,
      tol = .15
    )
  )
})

# Exponential models, BDB conservative----
test_that("mcmc_sample.Analysis() works for exponential BDB,
          conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  exp_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist(time_var = "time", cens_var = "cnsr"),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(exp_bdb_conservative$summary("HR_trt")[, "median"][[1]],
      0.57,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(exp_bdb_conservative$summary("HR_trt")[, "q5"][[1]],
      0.37,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(exp_bdb_conservative$summary("HR_trt")[, "q95"][[1]],
      0.83,
      tol = .02
    )
  )
})

# Exponential models, BDB aggressive----
test_that("mcmc_sample.Analysis() works for exponential BDB,
          aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  exp_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist(time_var = "time", cens_var = "cnsr"),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(exp_bdb_aggressive$summary("HR_trt")[, "median"][[1]],
      0.51,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(exp_bdb_aggressive$summary("HR_trt")[, "q5"][[1]],
      0.34,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(exp_bdb_aggressive$summary("HR_trt")[, "q95"][[1]],
      0.74,
      tol = .02
    )
  )
})

# Weibull models, BDB conservative----
test_that("mcmc_sample.Analysis() works for Weibull BDB,
          conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  weib_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(weib_bdb_conservative$summary("HR_trt")[, "median"][[1]],
      0.56,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(weib_bdb_conservative$summary("HR_trt")[, "q5"][[1]],
      0.36,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(weib_bdb_conservative$summary("HR_trt")[, "q95"][[1]],
      0.83,
      tol = .02
    )
  )
})

# Weibull models, BDB aggressive----
test_that("mcmc_sample.Analysis() works for Weibull BDB,
          aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  weib_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = weib_ph_surv_dist(
      time_var = "time",
      cens_var = "cnsr",
      shape_prior = normal_prior(0, 100000)
    ),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(weib_bdb_aggressive$summary("HR_trt")[, "median"][[1]],
      0.51,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(weib_bdb_aggressive$summary("HR_trt")[, "q5"][[1]],
      0.32,
      tol = .02
    )
  )

  expect_true(
    dplyr::near(weib_bdb_aggressive$summary("HR_trt")[, "q95"][[1]],
      0.74,
      tol = .02
    )
  )
})


# Logistic regression models, BDB conservative----
test_that("mcmc_sample.Analysis() works for logistic regression BDB,
          conservative borrowing", {
  skip_on_cran()
  skip_on_ci()
  bin_bdb_conservative <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(0.001, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(bin_bdb_conservative$summary("OR_trt")[, "median"][[1]],
      1.73,
      tol = .15
    )
  )

  expect_true(
    dplyr::near(bin_bdb_conservative$summary("OR_trt")[, "q5"][[1]],
      1.20,
      tol = .15
    )
  )

  expect_true(
    dplyr::near(bin_bdb_conservative$summary("OR_trt")[, "q95"][[1]],
      2.51,
      tol = .15
    )
  )
})

# Logistic regression models, BDB aggressive----
test_that("mcmc_sample.Analysis() works for logistic regression BDB,
          aggressive borrowing", {
  skip_on_cran()
  skip_on_ci()
  bin_bdb_aggressive <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = logistic_bin_outcome("resp"),
    borrowing = borrowing_details("BDB",
      normal_prior(0, 100000),
      ext_flag_col = "ext",
      tau_prior = gamma_prior(1, 0.001)
    ),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  ) %>%
    mcmc_sample(
      iter_warmup = 1000,
      iter_sampling = 10000,
      chains = 1
    )

  expect_true(
    dplyr::near(bin_bdb_aggressive$summary("OR_trt")[, "median"][[1]],
      1.62,
      tol = .15
    )
  )

  expect_true(
    dplyr::near(bin_bdb_aggressive$summary("OR_trt")[, "q5"][[1]],
      1.17,
      tol = .15
    )
  )

  expect_true(
    dplyr::near(bin_bdb_aggressive$summary("OR_trt")[, "q95"][[1]],
      2.29,
      tol = .15
    )
  )
})
