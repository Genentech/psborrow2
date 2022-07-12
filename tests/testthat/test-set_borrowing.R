test_that("set_borrowing works as expected", {

   # Create objects
   fb <- set_borrowing("Full borrowing")
   bdb <- set_borrowing("BDB","ext",normal_prior(0,1000),  normal_prior(0,1000))

   # Check classes
   expect_class(fb, "Borrowing")
   expect_class(bdb, 'Borrowing')

   # See that the columns were added
   expect_equal(fb@method, "Full borrowing")
   expect_class(bdb@tau_prior, "NormalPrior")

   # Errors
   expect_error(set_borrowing("Dr Dre"), "method must be within")
   expect_error(set_borrowing("Full borrowing", tau_prior = normal_prior(0,1000)), "no need to specify tau prior when method is not BDB")
   expect_error(set_borrowing("BDB"), "must be specified")

})
