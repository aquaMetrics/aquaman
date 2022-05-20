#' Assess survey data
#'
#' Assess if IQI survey data breaches mixing zone.
#'
#' @param data Data frame with survey data
#'
#' @return area in meters
#' @export
#'
#' @examples
#' \dontrun{
#' area <- assess(demo_data)
#' }
assess <- function(data) {

  probs <- probability_non_linear(data)
  overrides <- override(probs)
  breachs <- breach(overrides)
  areas <- area(breachs)
  return(areas$fifthPercentileArea)
}
