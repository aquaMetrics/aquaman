test_that("survey import works", {
  file <- system.file("extdat",
                      "survey-template/220421-SelfMon-N4952-CAV1-Enhanced.xlsx",
                      package = "aquaman"
  )
  data <- survey_import(file)
})
