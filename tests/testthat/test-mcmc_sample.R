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

test_that("mcmc_sample.Analysis() works for full borrowing, exponential dist", {

  full_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = "exponential"
    ))['trt'])

  full_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("Full borrowing",
                                  normal_prior(0, 100000),
                                  ext_flag_col = "ext"),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes <- mcmc_sample(full_exp_bayes_ao,
                                iter_warmup = 1000,
                                iter_sampling = 10000,
                                chains = 1)

  expect_true(
    dplyr::near(full_exp_bayes$summary('HR_trt')[,'median'][[1]],
                full_exp,
                tol = .02)
  )

})

test_that("mcmc_sample.Analysis() works for no borrowing, exponential dist", {

  int_exp <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[,"ext"]==0,]),
    dist = "exponential"
  ))['trt'])

  int_exp_bayes_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("No borrowing",
                                  normal_prior(0, 100000),
                                  ext_flag_col = "ext"),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  int_exp_bayes <- mcmc_sample(int_exp_bayes_ao,
                                iter_warmup = 1000,
                                iter_sampling = 10000,
                                chains = 1)

  expect_true(
    dplyr::near(int_exp_bayes$summary('HR_trt')[,'mean'][[1]],
                int_exp,
                tol = .02)
  )

})

test_that("mcmc_sample.Analysis() works for no borrowing, exponential dist,
          one covariate", {

  full_exp_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = "exponential"
  ))['trt'])

  full_exp_bayes_c1_ao <- create_analysis_obj(
    data_matrix = example_matrix,
    outcome = exp_surv_dist("time", "cnsr"),
    borrowing = borrowing_details("Full borrowing",
                                  normal_prior(0, 100000),
                                  ext_flag_col = "ext"),
    treatment = treatment_details("trt", normal_prior(0, 100000))
  )

  full_exp_bayes_c1 <- mcmc_sample(full_exp_bayes_c1_ao,
                                   iter_warmup = 1000,
                                   iter_sampling = 10000,
                                   chains = 1)

  expect_true(
    dplyr::near(full_exp_bayes$summary('HR_trt')[,'median'][[1]],
                full_exp,
                tol = .02)
  )



})

  full_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt,
    data = as.data.frame(example_matrix),
    dist = custom_weibullPH
  ))['trt'])

  full_weib_c1 <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt + cov1,
    data = as.data.frame(example_matrix),
    dist = custom_weibullPH
  ))['trt'])

  int_weib <- exp(coef(flexsurv::flexsurvreg(
    survival::Surv(time, 1-cnsr) ~ trt,
    data = as.data.frame(example_matrix[example_matrix[,"ext"]==0,]),
    dist = custom_weibullPH
  ))['trt'])

  full_log <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
    ))['trt'])

  full_log_c1 <- exp(coef(glm(
    resp ~ trt + cov1,
    data = as.data.frame(example_matrix),
    family = binomial(link = "logit")
  ))['trt'])

  int_log <- exp(coef(glm(
    resp ~ trt,
    data = as.data.frame(example_matrix[example_matrix[,"ext"]==0,]),
    family = binomial(link = "logit")
  ))['trt'])

  # Benchmark

})


custom_weibullPH <- list(name = "weibullPH",
                         pars = c("shape", "scale"), location = "scale",
                         transforms = c(log, log),
                         inv.transforms = c(exp, exp),
                         inits = function(t){
                           c(1,1)
                         })
