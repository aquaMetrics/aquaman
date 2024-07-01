test_that("test iqi() function works and is reproducible", {

  iqi <- iqi(demo_taxa)
  # check number of samples/length is always 10
  testthat::expect_equal(length(iqi), 10)
  # test same output each time (reproducible)
  iqi2 <- iqi(demo_taxa)
  test <- identical(iqi, iqi2)
  testthat::expect_true(test)

})
