#' @importFrom rappdirs user_data_dir
file_download <- function() {
  data_dir <- function() {
    getOption("aquaman.data_dir", default = user_data_dir("aquaman"))
  }

  fpath <- paste0(data_dir(), "/silva_nr99_v138.1_train_set.fa.gz")

  if (!file.exists(fpath)) {
    ask <- function(...) {
      choices <- c("Yes", "No")
      cat(paste0(..., collapse = ""))
      utils::menu(choices) == which(choices == "Yes")
    }

    check_write_to_data_dir <- function(dir, ask) {
      if (ask) {
        ans <- ask(paste("Aquaman would like to store reference taxonomy
                         'silva_nr99_v138.1_train_set' in the directory:",
          dir, "Is that okay?",
          sep = "\n"
        ))
        if (!ans) stop("Exiting...", call. = FALSE)
      }
    }

    check_write_to_data_dir(data_dir(), ask = TRUE)
    dir.create(data_dir())
    options(timeout = max(800, getOption("timeout")))

    url <-
      "https://zenodo.org/record/4587955/files/silva_nr99_v138.1_train_set.fa.gz?download=1"
    utils::download.file(url, fpath)
  }

  return(fpath)
}
