# Create covariance matrices----
set.seed(123)
mat3_1 <- matrix(rWishart(1, 3, diag(3)), ncol = 3)
mat3_2 <- matrix(rWishart(1, 3, diag(3)), ncol = 3)
mat8_1 <- matrix(rWishart(1, 8, diag(8)), ncol = 8)
mat8_2 <- matrix(rWishart(1, 8, diag(8)), ncol = 8)
mat_custom <- matrix(
  c(
    1, 2, 3,
    2, 1, 2,
    3, 2, 1
  ),
  byrow = TRUE,
  ncol = 3
)
mat_custom2 <- mat_custom * 2

# Objects are created successfully----
test_that("SimCovariates objects created correctly", {
  expect_class(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov2 = bin_var(0.8, 0.2),
        cov3 = cont_var(100, 200)
      ),
      covariance_internal = mat3_1,
      covariance_external = mat3_2
    ),
    "SimCovariates"
  )

  expect_class(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov2 = bin_var(0.8, 0.2),
        cov3 = cont_var(100, 200),
        cov4 = bin_var(.1, .1),
        cov5 = cont_var(-10000, 10000),
        cov6 = cont_var(-20, -30),
        cov7 = bin_var(0.001, 0.001),
        cov8 = bin_var(.5, .5)
      ),
      covariance_internal = mat8_1,
      covariance_external = mat8_2
    ),
    "SimCovariates"
  )
})

test_that("Invalid sim_covariate() covariates inputs are caught", {
  # Unnamed covariate list
  expect_error(
    sim_covariates(
      covariates = list(
        bin_var(0.9, 0.9),
        bin_var(0.8, 0.2),
        cont_var(100, 200)
      ),
      covariance_internal = mat3_1,
      covariance_external = mat3_2
    ),
    "All covariates must be named"
  )

  # Semi-named covariate list
  expect_error(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        bin_var(0.8, 0.2),
        cont_var(100, 200)
      ),
      covariance_internal = mat3_1,
      covariance_external = mat3_2
    ),
    "All covariates must be named"
  )

  # Duplicate names
  expect_error(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov1 = bin_var(0.8, 0.2),
        cov2 = cont_var(100, 200)
      ),
      covariance_internal = mat3_1,
      covariance_external = mat3_2
    ),
    "named arguments to list for argument `covariates` must all be different"
  )
})


test_that("Invalid sim_covariate() matrix inputs are caught", {
  # Wrong dimensions
  expect_error(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov2 = bin_var(0.8, 0.2),
        cov3 = cont_var(100, 200)
      ),
      covariance_internal = mat8_1,
      covariance_external = mat8_2
    ),
    "Covariance matrices must be symmetric square matrices"
  )

  # Not symmetric
  mat_assym <- matrix(
    c(
      1, 2, 3,
      2, 1, 2,
      3, 3, 1
    ),
    byrow = TRUE,
    ncol = 3
  )

  expect_error(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov2 = bin_var(0.8, 0.2),
        cov3 = cont_var(100, 200)
      ),
      covariance_internal = mat_assym,
      covariance_external = mat_assym
    ),
    "Covariance matrices must be symmetric square matrices"
  )

  # Not semi positive definite
  semi_neg_eigen_mat <- matrix(
    c(
      .99, .78, .59, .44,
      .78, .92, .28, .81,
      .59, .28, 1.12, .23,
      .44, .81, .23, .99
    ),
    ncol = 4,
    byrow = TRUE
  )

  expect_error(
    sim_covariates(
      covariates = list(
        cov1 = bin_var(0.9, 0.9),
        cov2 = bin_var(0.8, 0.2),
        cov3 = cont_var(100, 200),
        cov4 = bin_var(0.8, 0.9)
      ),
      covariance_internal = semi_neg_eigen_mat,
      covariance_external = semi_neg_eigen_mat
    ),
    "Covariance matrices must be semi positive definite"
  )
})
