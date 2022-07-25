test_that("area function output matches Spotfire script", {
  skip("too long")
  area <- assess(demo_iqi)
})


test_that("test reintraid 2020 against reported results", {
  data <- read.csv(
    system.file("extdat",
                "test-data/2022-reintraid.csv",
                package = "aquaman"
    ), check.names = FALSE
  )

  data <- dplyr::rename(data, "Survey_date" = Survey_Date)
  area <- assess(data)
  # Current spotfire testing
  testthat::expect_equal(round(area[[1]], 0),   35780)
  # Result reported - change to area calculator code since Feb 2022?
  # testthat::expect_equal(round(area, 0), 37070)
})
