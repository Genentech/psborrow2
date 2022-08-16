test_that("h_glue works as expected", {
  number <- 3.141592653587
  string <- "abc"
  expect_equal(h_glue("pi is {{number}}"), "pi is 3.141592653587")
  expect_equal(h_glue("{{1+2}}", "{{string}}"), "3abc")
  expect_equal(h_glue("{string}"), "{string}")
})
