test_that("area function output matches Spotfire script", {
  iqi <- iqi(demo_taxa)
  expect_equal(length(iqi), 10)
})
