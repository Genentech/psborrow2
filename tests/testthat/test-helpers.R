test_that("h_glue works as expected", {
  number <- 3.141592653587
  string <- "abc"
  expect_equal(h_glue("pi is {{number}}"), "pi is 3.141592653587")
  expect_equal(h_glue("{{1+2}}", "{{string}}"), "3abc")
  expect_equal(h_glue("{string}"), "{string}")
})


test_that("parse_constraint works as expected", {
  expect_equal(
    parse_constraint("<lower=1>"),
    c(lower = 1, upper = Inf)
  )

  expect_equal(
    parse_constraint("<lower=0, upper=1>"),
    c(lower = 0, upper = 1)
  )

  expect_equal(
    parse_constraint(""),
    c(lower = -Inf, upper = Inf)
  )
})


test_that("parse_constraint works as expected with prior list", {
  object <- add_covariates(
    c("cov1", "cov2", "cov3"),
    list(
      normal_prior(0, 10),
      beta_prior(0.3, 0.3),
      gamma_prior(30, 1)
    )
  )
  result <- get_covariate_constraints(object)
  expect_equal(
    result,
    matrix(c(-Inf, 0, 0, Inf, 1, Inf), ncol = 2, dimnames = list(NULL, c("lower", "upper")))
  )
})

test_that("parse_constraint works as expected with single prior", {
  object <- add_covariates(c("cov1", "cov2", "cov3"), normal_prior(0, 100))
  result <- get_covariate_constraints(object)
  expect_equal(
    result,
    matrix(
      c(-Inf, Inf),
      ncol = 2,
      nrow = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("lower", "upper"))
    )
  )
})
