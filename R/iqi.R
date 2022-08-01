#' Predict IQI score
#'
#' WORK IN PROGRESS - DO NOT USE IN PRODUCTION - Using family-level bacteria
#' taxa as predictors, calculate benthic invert IQI as an outcome. Model created
#' by Tom Wilding (SAMS), based on training data from SEPA And MOWI.
#'
#' @param predictors Data frame of family-level bacteria taxa.
#'
#' @return dataframe of IQI scores
#' @export
#' @import RRF
#' @examples
#' \dontrun{
#' iqi_scores <- iqi(demo_taxa)
#' }
iqi <- function(predictors) {
  warning("WORK IN PROGRESS - DO NOT USE IN PRODUCTION")
  outcome <- predict(aquaman::iqi_model, predictors)
  return(outcome)
}
