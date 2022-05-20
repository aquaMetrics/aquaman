test_that("breach function matches spotfire script outputs", {
  data <- probability_non_linear(demo_data)
  breach <- breach(data)
  breach_positions <- breach[["breachPositionEnsemble"]]
  breach_best_fit_test <- breach[["breachPositionBestFit"]]
  # test data is from Spotfire / R script
  breach_positions_test <- read.csv(
    system.file("extdat",
                "test-data/2021-05-26-bellister/breach-positions.csv",
                package = "aquaman"
    )
  )
  breach_positions_test <- breach_positions_test[complete.cases(breach_positions_test), ]

  breach_best_fit_test <- read.csv(
    system.file("extdat",
                "test-data/2021-05-26-bellister/breach-best-fit.csv",
                package = "aquaman"
    )
  )

  breach_positions$Transect <- as.integer(breach_positions$Transect)
  breach_positions <- data.frame(breach_positions)
  row.names(breach_positions) <- NULL
  row.names(breach_positions_test) <- NULL
  expect_equal(breach_positions, breach_positions_test)
  expect_equal(breach_best_fit_test, breach_best_fit_test)
})
