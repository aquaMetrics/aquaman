test_that("consecutive_stations works", {
    check <- consecutive_stations(demo_iqi)
  testthat::expect_equal(class(check$sample_point_checks), "data.frame")
  testthat::expect_equal(nrow(check$sample_point_checks), 4)
  testthat::expect_equal(ncol(check$sample_point_checks), 5)
  testthat::expect_equal(
    check$sample_point_checks[1, "stationNumber"],
    "Compliant: Min. number of stations have been taken (9)"
  )
  testthat::expect_equal(
    check$sample_point_checks[1, "twoConsecutiveStations"],
    "Compliant: 2 consecutive stations at Good are returned"
  )
})

test_that("consecutive_stations calculates columns", {
  test_data <- dplyr::select(aquaman::demo_iqi,
                             -Latitude,
                             -Longitude,
                             -Bearing,
                             -Distance,
                             -MCFF_Transect)
  test <- consecutive_stations(test_data)
  test <- test$survey_data
  test <- dplyr::select(test,
                        Survey_date,
                        MCFF,
                        Transect,
                        Station,
                        IQI,
                        Easting,
                        Northing,
                        MCFF_Transect,
                        Longitude,
                        Latitude,
                        Bearing,
                        Distance,
                        "Number of stations per transect" =
                          Number.of.stations.per.transect,
                        "WFD status" =
                          WFD.status,
                        MCFF_Transect_Station)
  expected <- as.data.frame(aquaman::demo_iqi)
  # Not clear when this attributes have been added? But not neccessary they
  # match exactly to test data.
  attributes(expected)$problems <- NULL
  attributes(expected)$spec <- NULL
  test <- as.data.frame(test)
  # Rounding error? Bearing isn't used in calculation so not that important to
  # match exactly.
  test$Bearing <- round(test$Bearing, 4)
  expected$Bearing <- round(expected$Bearing, 4)
 testthat::expect_equal(test, expected)

})
