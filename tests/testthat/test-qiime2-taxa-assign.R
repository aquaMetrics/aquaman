test_that("qiime2 taxa assign works", {
  skip("long running test")
  path = "OneDrive - Scottish Environment Protection Agency/Reports/Fish Farm/operating_iqi_surveys/ScreeningTool_SequenceData"
  qiime2_taxa_assign(path)
})
