#' Predict IQI score
#'
#' Based on family-level bacteria taxa predictors calculate the benthic invert
#' IQI outcome.
#'
#' @param predictors Data frame of family-level bacteria taxa.
#'
#' @return dataframe of IQI scores
#' @export
#'
#' @examples
#' \dontrun{
#' iqi_scores <- iqi(demo_taxa)
#' }
iqi <- function(predictors) {
  outcome <- predict(iqi_model, demo_taxa)
  return(outcome)
}
