test_that("area function output matches Spotfire script", {
  skip("too long")
  area <- assess(demo_iqi)
})


test_that("test reintraid 2020 against reported results", {
  skip("temporary internal test data")
  library(readxl)
  # reintraid <- read_excel("~/Downloads/2020-graph-reintraid.xlsx",
  #                         sheet = "Spotfire")
  # reintraid <- dplyr::rename(reintraid, "MCFF" = Site_name)
  reintraid <- read_excel("~/Downloads/survey-iqi-spotfire-reintraid.xlsx")
  reintraid <- dplyr::filter(reintraid, MCFF == "Reintraid")
  # reintraid <- arrange(reintraid, Transect, Station)
  reintraid <- rename(reintraid,
                      "Survey_date" =  Survey_Date
                      )
  areas <- assess(reintraid
                 )
  # Test not matching - change to area calculator code since Feb 2022?
  testthat::expect_equal(round(areas, 0), 37070)
})
