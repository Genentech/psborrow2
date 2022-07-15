test_that("LogisticBinaryEndpoint distribution is rendering correctly", {

   # Binomail endpoint class
   bin_endpoint <- logistic_bin_endpoint()

   # Expect correct class
   expect_class(bin_endpoint, "LogisticBinaryEndpoint")
   expect_equal(bin_endpoint@n_param, 0L)

   # Errors
   expect_error(logistic_bin_endpoint(3),
                regexp = "unused argument"
   )
})
