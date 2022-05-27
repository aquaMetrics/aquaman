test_that("consecutive_stations works", {
  check <- consecutive_stations(demo_iqi)
  testthat::expect_equal(class(check), "data.frame")
  testthat::expect_equal(nrow(check), 4)
  testthat::expect_equal(ncol(check), 5)
  testthat::expect_equal(
    check[1, "stationNumber"],
    "Compliant: Min. number of stations have been taken (9)"
  )
  testthat::expect_equal(
    check[1, "twoConsecutiveStations"],
    "Compliant: 2 consecutive stations at Good are returned"
  )
})
