test_that("override works", {

  probability <- readRDS(
    system.file("extdat",
                "test-data/2021-05-26-bellister/test-probability.rds",
                package = "aquaman"
    )
  )

  override <- override(probability)
  expect_equal(class(override),"list")
})
