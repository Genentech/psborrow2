test_that("sim_samplesize() is catching incorrect inputs", {
  expect_error(
    sim_samplesize("one", "two", "three"),
    "Must be of type 'single integerish value'"
  )

  expect_error(
    sim_samplesize(1.2, 2.9, 3.1),
    "Must be of type 'single integerish value'"
  )

  expect_error(
    sim_samplesize(100, 0, 1000),
    "n_external_control must be >0"
  )

  expect_error(
    sim_samplesize(-1, 100, 1000),
    "n_internal_control must be >0"
  )

  expect_error(
    sim_samplesize(1, 100, -20),
    "n_external_experimental must be >0"
  )
})

test_that("sim_samplesize() is producing the correct counts", {
  nic_in <- 101
  nec_in <- 102
  nie_in <- 103

  samplesize <- sim_samplesize(nic_in, nec_in, nie_in)
  expect_class(samplesize, "SimSampleSize")

  nic_out <- sum(samplesize@mat[, "trt"] == 0 & samplesize@mat[, "ext"] == 0)
  nec_out <- sum(samplesize@mat[, "trt"] == 0 & samplesize@mat[, "ext"] == 1)
  nie_out <- sum(samplesize@mat[, "trt"] == 1 & samplesize@mat[, "ext"] == 0)

  expect_equal(nic_in, nic_out)
  expect_equal(nec_in, nec_out)
  expect_equal(nie_in, nie_out)
})
