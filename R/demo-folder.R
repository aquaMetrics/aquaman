#' Demo DNA folder
#'
#' Path to folder containing DNA and meta data held within `aquaman` package.
#'
#' @return path vector
#' @export
#'
#' @examples
#' \dontrun{
#' path <- demo_path()
#' }
demo_path <- function() {
  folder <- system.file("extdat",
    "dna-data",
    package = "aquaman"
  )
  return(folder)
}
