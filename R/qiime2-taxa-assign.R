#' Qiime2 taxa assign
#'
#' Only on Linux, Mac or WSL
#'
#' @param path Path to directory containing .fasta files
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "my_data_folder/"
#' qiime2_taxa_assign(path)
#' }

qiime2_taxa_assign <- function(path = NULL) {

  script_path <- system.file("extdat",
                             "qiime2-bash-script", package = "aquaman")
  # Only on Linux, Mac or WSL
  system(paste0("bash ", script_path, "/qiime2-taxa-assign.txt ", path))

}
