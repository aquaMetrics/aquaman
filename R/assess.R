#' Assess survey data
#'
#' Assess if IQI survey data breaches mixing zone.
#'
#' @param data Data frame with survey data
#' @param overrideTransect1 Optional override distance Transect 1
#' @param overrideTransect2 Optional override distance Transect 2
#' @param overrideTransect3 Optional override distance Transect 3
#' @param overrideTransect4 Optional override distance Transect 4
#' @param overrideBearing1 Optional override bearing Transect 1
#' @param overrideBearing2 Optional override bearing Transect 2
#' @param overrideBearing3 Optional override bearing Transect 3
#' @param overrideBearing4 Optional override bearing Transect 4
#' @return area in meters
#' @export
#'
#' @examples
#' \dontrun{
#' area <- assess(demo_iqi)
#' }
assess <- function(data,
                   overrideTransect1 = NA,
                   overrideTransect2 = NA,
                   overrideTransect3 = NA,
                   overrideTransect4 = NA,
                   overrideBearing1 = NA,
                   overrideBearing2 = NA,
                   overrideBearing3 = NA,
                   overrideBearing4 = NA) {

  data <- consecutive_stations(data)
  probs <- probability_non_linear(data$survey_data)
  overrides <- override(probs,
                        overrideTransect1,
                        overrideTransect2,
                        overrideTransect3,
                        overrideTransect4,
                        overrideBearing1,
                        overrideBearing2,
                        overrideBearing3,
                        overrideBearing4)
  breachs <- breach(overrides)
  areas <- area(breachs)
  return(areas$fifthPercentileArea)
}
