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
#' area <- assess(demo_iqi)
#' }
assess <- function(data) {
  probs <- probability_non_linear(data)
  # overrides <- override(probs
  #                        ,
  #                        overrideTransect4 = 68
  #                       )
  breachs <- breach(probs)
  areas <- area(breachs)
  return(areas$fifthPercentileArea)
}
