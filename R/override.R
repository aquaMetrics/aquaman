#' Override Distance and Bearing for Transects
#'
#' @param data named list of 4 data frames created by the
#'   `probability_non_linear` function. These are:  `data` (survey), `geoDf`
#'   (distances to good),`geoDfBestFit` (best fit distance) to good and
#'   `hexdfOut` (hexagon heat map).
#' @param overrideTransect1 Override distance Transect 1
#' @param overrideTransect2 Override distance Transect 2
#' @param overrideTransect3 Override distance Transect 3
#' @param overrideTransect4 Override distance Transect 4
#' @param overrideBearing1 Override bearing Transect 4
#' @param overrideBearing2 Override bearing Transect 4
#' @param overrideBearing3 Override bearing Transect 4
#' @param overrideBearing4 Override bearing Transect 4
#'
#' @return area in meters
#' @export
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' probability <- probability_non_linear(demo_iqi)
#' probability <- override(probability)
#' }
override <- function(data,
                     overrideTransect1 = NA,
                     overrideTransect2 = NA,
                     overrideTransect3 = NA,
                     overrideTransect4 = NA,
                     overrideBearing1 = NA,
                     overrideBearing2 = NA,
                     overrideBearing3 = NA,
                     overrideBearing4 = NA) {

  # Split out the tables from the input `data` list ---------------------------
  inSurveyData <- data[["data"]]
  geoDf <- data[["geoDf"]]
  geoDfBestFit <- data[["geoDfBestFit"]]
  hexdfOut <- data[["hexdfOut"]]

  # Prepare the input in list to make looping / checking easier ---------------
  transect_overrides <- data.frame("override" = c(
    overrideTransect1,
    overrideTransect2,
    overrideTransect3,
    overrideTransect4
  ))
  transect_overrides$transect <- 1:nrow(transect_overrides)
  transect_overrides$name <- "distance"
  bearing_overrides <- data.frame("override" = c(
    overrideBearing1,
    overrideBearing2,
    overrideBearing3,
    overrideBearing4
  ))

  bearing_overrides$transect <- 1:nrow(bearing_overrides)
  bearing_overrides$name <- "bearing"
  overrides <- bind_rows(transect_overrides, bearing_overrides)
  overrides$id <- paste0(overrides$name, overrides$transect)

  # Override values -----------------------------------------------------------
  map_df(split(overrides, overrides$id), function(row) {
    if (!is.na(row$override)) {
      if (row$name == "distance") {
        geoDf$D2Ghist[geoDf$Transect == row$transect] <<-
          as.character(row$override)
        geoDf$D2G[geoDf$Transect == row$transect] <<-
          as.character(row$override)
        geoDfBestFit$D2G[geoDf$Transect == row$transect] <<-
          as.character(row$override)
      }
      if (row$name == "bearing") {
        geoDf$Bearing[geoDf$Transect == row$transect] <<-
          as.character(row$override)
        geoDfBestFit$Bearing[geoDf$Transect == row$transect] <<-
          as.character(row$override)
      }
    }
  })

  # Return values -------------------------------------------------------------
  data <- list(
    inSurveyData,
    geoDf,
    geoDfBestFit,
    hexdfOut
  )
  names(data) <- c("data", "geoDf", "geoDfBestFit", "hexdfOut")
  return(data)
}
