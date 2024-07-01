test_that("check reproducible", {

  import_taxa <- aquaman::import_qiime2(
    path =
      system.file("extdat/raw-taxa-data/S16_TestMOWI.csv", package = "aquaman")
    )

  import_taxa_2 <- aquaman::import_qiime2(
    path =
      system.file("extdat/raw-taxa-data/S16_TestMOWI.csv", package = "aquaman")
  )

  # By default import_qiime2() always the same output
  test_equal <- all.equal(import_taxa, import_taxa_2)
  testthat::expect_true(test_equal)

  # If seed set to NULL then randomness in rrarefy
  import_taxa_3 <- aquaman::import_qiime2(
    path =
      system.file("extdat/raw-taxa-data/S16_TestMOWI.csv", package = "aquaman")
  , seed = NULL)
  test_different <- identical(import_taxa, import_taxa_3)
  testthat::expect_false(test_different)


})
