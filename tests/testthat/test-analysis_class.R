test_that("get_vars works for Analysis", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt", "cov1", "cov2")
  )

  object@covariates <- NULL
  expect_equal(
    get_vars(object),
    c(time_var = "time", cens_var = "cnsr", ext_flag_col = "ext", trt_flag_col = "trt")
  )
})

test_that("show works for Analysis", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_snapshot(show(object))
})

test_that("show works without covariates", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  expect_snapshot(show(object))
})


test_that("show works with no borrowing", {
  object <- .analysis_obj(
    data_matrix = example_matrix,
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_none(
      ext_flag_col = "ext"
    )
  )
  expect_snapshot(show(object))
})


test_that("get_stan_code works when cmdstanr isn't available", {
  skip_if(check_cmdstan())
  expect_warning(
    object <- create_analysis_obj(
      data_matrix = example_matrix,
      covariates = add_covariates(
        c("cov1", "cov2"),
        prior_normal(0, 1000)
      ),
      outcome = outcome_surv_exponential(
        time_var = "time",
        cens_var = "cnsr",
        baseline_prior = prior_normal(0, 1000)
      ),
      treatment = treatment_details(
        "trt",
        prior_normal(0, 1000)
      ),
      borrowing = borrowing_full("ext")
    ),
    "cmdstanr is not available"
  )
  result <- get_stan_code(object)
  expected <- "functions {

}

  data {
  int<lower=0> N;
  vector[N] trt;
  vector[N] time;
vector[N] cens;


  int<lower=0> K;
matrix[N, K] X;
vector[K] L_beta;
vector[K] U_beta;
}

  parameters {
real beta_trt;


real alpha;
vector<lower=L_beta, upper=U_beta>[K] beta;
}

  transformed parameters {
  real HR_trt = exp(beta_trt);
}

  model {
  vector[N] lp;
  vector[N] elp;
  beta_trt ~ normal(0, 1000);
  lp = alpha + X * beta + trt * beta_trt ;
elp = exp(lp) ;

  beta[1] ~ normal(0, 1000) ;
beta[2] ~ normal(0, 1000) ;
  alpha ~ normal(0, 1000) ;
  for (i in 1:N) {
   if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i] );
   } else {
      target += exponential_lpdf(time[i] | elp[i] );
   }
}
}"
  expect_string(result, expected)
})


test_that("get_stan_code works when cmdstanr is available", {
  skip_if_not(check_cmdstan())
  object <- create_analysis_obj(
    data_matrix = example_matrix,
    covariates = add_covariates(
      c("cov1", "cov2"),
      prior_normal(0, 1000)
    ),
    outcome = outcome_surv_exponential(
      time_var = "time",
      cens_var = "cnsr",
      baseline_prior = prior_normal(0, 1000)
    ),
    treatment = treatment_details(
      "trt",
      prior_normal(0, 1000)
    ),
    borrowing = borrowing_full("ext")
  )
  result <- get_stan_code(object)
  expected <- "functions {

}

  data {
  int<lower=0> N;
  vector[N] trt;
  vector[N] time;
vector[N] cens;


  int<lower=0> K;
matrix[N, K] X;
vector[K] L_beta;
vector[K] U_beta;
}

  parameters {
real beta_trt;


real alpha;
vector<lower=L_beta, upper=U_beta>[K] beta;
}

  transformed parameters {
  real HR_trt = exp(beta_trt);
}

  model {
  vector[N] lp;
  vector[N] elp;
  beta_trt ~ normal(0, 1000);
  lp = alpha + X * beta + trt * beta_trt ;
elp = exp(lp) ;

  beta[1] ~ normal(0, 1000) ;
beta[2] ~ normal(0, 1000) ;
  alpha ~ normal(0, 1000) ;
  for (i in 1:N) {
   if (cens[i] == 1) {
      target += exponential_lccdf(time[i] | elp[i] );
   } else {
      target += exponential_lpdf(time[i] | elp[i] );
   }
}
}"
  expect_string(result, expected)
})
