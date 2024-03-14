# DataSimEnrollment -------
test_that("custom_enrollment constructor works as expected", {
  result <- custom_enrollment(
    fun = function(n) rpois(n, lambda = 5),
    label = "Poisson enrollment distribution"
  )
  expect_class(result, "DataSimEnrollment")
  expect_string(result@label, "Poisson enrollment distribution")
  set.seed(100)
  expect_equal(result@fun(5), c(4, 3, 5, 2, 5))
})

test_that("custom_enrollment constructor fails as expected", {
  expect_error(
    custom_enrollment(fun = function(num) rpois(num, lambda = 5), label = "test"),
    "formal arguments"
  )
  expect_error(
    custom_enrollment(fun = function(n) rpois(n, lambda = 5), label = NA),
    "label"
  )
})
