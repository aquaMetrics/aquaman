#' Assign Taxa
#'
#' WORK IN PROGRESS - DO NOT USE IN PRODUCTION - based on dada2 tutorial
#' https://benjjneb.github.io/dada2/tutorial.html. Assign family taxa from
#' .fastq files. Will prompt you to download taxonomic reference file on first
#' use or user can provide `taxonomy_file = '...'` path.
#'
#' @param path to folder containing folder(s) of fastq files. Assumes .fastq
#'   files have format `SAMPLENAME_XXXX.fastq` - XXXX... Must Be 'L001_R1_001'
#'   or L001_R2_001' as per normal Illumina naming. SAMPLENAME Must Be unique at
#'   least within the path.
#' @param folder_pattern Optional Character string to match against folders to
#'   analyse. Often "001" can be used to identify Illumina generated folders.
#' @param taxonomy_file Optional path to taxonomy file e.g.
#'   ...silva_nr99_v138.1_train_set.fa.gz, otherwise will try to download file.
#' @param multithread Set to FALSE on windows. Only applies to
#'   dada2::filterAndTrim
#'   function `https://github.com/benjjneb/dada2/issues/1100`. The rest of the
#'   DADA2 functions will use multithread even on windows.
#' @param tax_level The taxonomic levels being assigned. Default is  "Family",
#'   options are c("Kingdom", "Phylum", "Class", "Order", "Family", "genus",
#'   "Species").
#' @param save_output Boolean to indicating if taxa assigment data is
#'   saved/appended to 'output.csv' should be saved in the path after each
#'   folder. This can save progress if errors or crashes happen before
#'   completion.
#' @return dataframe containing assigned fmaily level taxa
#' @export
#' @importFrom dada2 filterAndTrim learnErrors dada mergePairs makeSequenceTable getSequences removeBimeraDenovo assignTaxonomy plotErrors getUniques
#' @importFrom dplyr select everything rename starts_with contains group_by summarize_all across
#' @importFrom purrr map_df
#' @importFrom utils head capture.output
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' taxa <- assign_taxa(path = demo_path())
#' }
#'
assign_taxa <- function(path = NULL,
                        folder_pattern = NULL,
                        taxonomy_file = NULL,
                        tax_level = NULL,
                        multithread = FALSE,
                        save_output = FALSE) {
  warning("WORK IN PROGRESS - DO NOT USE IN PRODUCTION")
  set.seed(100) # Initialize random number generator for reproducibility
  # Read files ---------------------------------------------------
  # List all directories in path
  dirs <- sort(list.dirs(path, full.names = TRUE))
  # exclude parent directory
  dna_dirs <- dirs[2:length(dirs)]


  # Only folders matching user defined pattern
  if (!is.null(folder_pattern)) {
    dna_dirs <- unlist(lapply(folder_pattern, function(pattern) {
      dna_dirs <- dna_dirs[grep(pattern, dna_dirs)]
    }))
  }
  # Check no repeating file names
  all_files <- purrr::map(dna_dirs, function(dir) {
    files <- list.files(dir, pattern = ".fastq")
    return(files)
  })
  all_files <- unlist(all_files)
  duplicate_files <- all_files[duplicated(all_files)]
  if (length(duplicate_files) > 0) {
    paste_duplicate_files <- paste(duplicate_files, collapse = "\n")
    warning(cat(
      paste("You provided a dna data path:\n ",
        path, "\n",
        "However, this path contains duplicated files names in the folder or subfolders.\n",
        "The duplicate file names:\n",
        paste_duplicate_files,
        sep = "",
        collapse = "\n"
      )
    ), call. = FALSE)
  }
  # Forward and reverse fastq filenames have format:
  # SAMPLENAME_R1_001.fastq
  # SAMPLENAME_R2_001.fastq
  # Loop through files --------------------------------------------------------
  taxa <- map_df(dna_dirs, function(dir) {
    fn_fs <- sort(list.files(dir, pattern = "_R1_001.fastq", full.names = TRUE))
    fn_rs <- sort(list.files(dir, pattern = "_R2_001.fastq", full.names = TRUE))
    # Remove files which are duplicated.
    if(length(duplicate_files) > 0) {
    fn_fs <- purrr::map(fn_fs, function(file) {
      f <- purrr::map(duplicate_files, function(dup) {
        # if(file == "")
        file <- file[grepl(dup, file) == FALSE]
      })
    })
    fn_fs <- unlist(fn_fs)
    fn_fs <- unique(fn_fs)
    fn_rs <- purrr::map(fn_rs, function(file) {
      f <- purrr::map(duplicate_files, function(dup) {
        file <- file[grepl(dup, file) == FALSE]
      })
    })
    fn_rs <- unlist(fn_rs)
    fn_rs <- unique(fn_rs)
    }
    # Sense check -------------------------------------------------------------
    if (length(fn_fs) == 0 && length(fn_rs) == 0) {
      warning(paste0(
        "You provided a folder: ",
        dir,
        " which does not contain any .fastq file names containing the _R2_001 or
        R2_001 pattern"
      ))
      return(NULL)
    }

    if (length(fn_fs) != length(fn_rs)) {
      warning(paste0(
        "You provided a folder: ",
        dir,
        " which contains unequal number of forward and reverse .fastq files"
      ))
      return(NULL)
    }

    # Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
    sample_names <- sapply(strsplit(basename(fn_fs), "_L001"), `[`, 1)

    # dada2::plotQualityProfile(fn_fs[1:2])

    # Filtered files
    filt_fs <- file.path(
      path, "filtered",
      paste0(sample_names, "_F_filt.fastq.gz")
    )
    filt_rs <- file.path(
      path, "filtered",
      paste0(sample_names, "_R_filt.fastq.gz")
    )
    names(filt_fs) <- sample_names
    names(filt_rs) <- sample_names

    # Trim -----------------------------------------------------------
    # Remove primers, see trimLeft parameter.
    # nchar("CCTACGGGNGGCWGCAG")
    # nchar("GACTACHVGGGTATCTAATCC")
    out <- filterAndTrim(fn_fs,
      filt_fs,
      fn_rs,
      filt_rs,
      truncLen = c(160, 160),
      maxN = 0,
      maxEE = c(8, 8),
      truncQ = 2,
      rm.phix = TRUE,
      compress = TRUE,
      trimLeft = c(17, 21),
      multithread = multithread
    )
    message(print(head(out)))

    err_f <- learnErrors(filt_fs, multithread = TRUE)
    err_r <- learnErrors(filt_rs, multithread = TRUE)
    plotErrors(err_f, nominalQ = TRUE)

    dada_fs <- dada(filt_fs, err = err_f, multithread = TRUE)
    dada_rs <- dada(filt_rs, err = err_r, multithread = TRUE)


    dada_fs[[1]]

    mergers <- mergePairs(dada_fs, filt_fs, dada_rs, filt_rs, verbose = TRUE)
    # Inspect the merger data.frame from the first sample
    head(mergers[[1]])

    # Deleted filtered files after use (save disk space...)
    if (file.exists(filt_fs[1])) {
      unlink(file.path(path, "filtered"), recursive = TRUE)
      cat("The filtered directory is deleted")
    }

    seq_table <- makeSequenceTable(mergers)
    message(print(dim(seq_table)))
    message(print(table(nchar(getSequences(seq_table)))))

    seq_table_nochim <- removeBimeraDenovo(seq_table,
      method = "consensus",
      multithread = TRUE,
      verbose = TRUE
    )
    message(print(dim(seq_table_nochim)))
    message(print(sum(seq_table_nochim) / sum(seq_table)))

    # If processing a single sample, remove the sapply calls: e.g. replace
    # sapply(dada_fs, get_n) with get_n(dada_fs)
    if (length(filt_fs) == 1) {
      get_n <- function(x) sum(getUniques(x))
      track <- cbind(
        out,
        get_n(dada_fs),
        get_n(dada_rs),
        get_n(mergers),
        rowSums(seq_table_nochim)
      )
    } else {
      # If processing a multiple samples, use sapply(dada_fs, get_n)
      get_n <- function(x) sum(getUniques(x))
      track <- cbind(
        out,
        sapply(dada_fs, get_n),
        sapply(dada_rs, get_n),
        sapply(mergers, get_n),
        rowSums(seq_table_nochim)
      )
    }


    colnames(track) <- c(
      "input",
      "filtered",
      "denoisedF",
      "denoisedR",
      "merged",
      "nonchim"
    )
    rownames(track) <- sample_names
    message(print(head(track)))

    # Download reference taxonomy: silva_nr99_v138.1_train_set.fa.gz
    # Stores files locally.
    if (is.null(taxonomy_file)) {
      taxonomy_file <- file_download()
    }

    taxa <- assignTaxonomy(
      seqs = seq_table_nochim,
      refFasta = taxonomy_file,
      multithread = TRUE
    )
    # Merge number of reads and taxa name in single table
    sequences <- data.frame("reads" = t(seq_table_nochim))
    # Merge by row.names
    taxa_sequences <- merge(taxa, sequences, by = 0)
    taxa_sequences <- select(taxa_sequences, -.data$Row.names)
    message(paste(capture.output(head(taxa_sequences, 4)), collapse = "\n"))
    # Only want column that matches tax_level e.g. "Family"
    if (!is.null(tax_level)) {
      name <- names(taxa_sequences)[grep(tax_level,
        names(taxa_sequences),
        ignore.case = TRUE
      )]
      taxa_sequences <- select(
        taxa_sequences,
        starts_with(name),
        contains("reads")
      )

      taxa_sequences <- taxa_sequences[complete.cases(taxa_sequences), ]

      taxa_sequences <- group_by(taxa_sequences, across(name))
      taxa_sequences <- summarize_all(taxa_sequences, sum)
      names(taxa_sequences) <- c(name, sample_names)
    } else {
      tax_level <- c(
        "Kingdom",
        "Phylum",
        "Class",
        "Order",
        "Family",
        "Genus"
      )
      names(taxa_sequences) <- c(
        tax_level,
        sample_names
      )
    }
    # save output after each assignment
    if (save_output == TRUE) {
      file <- file.path(path, "output.csv")
      if (file.exists(file)) {
        # output.csv exists so append data
        taxa_file <- utils::read.csv(file, check.names = FALSE)
        taxa_file <- dplyr::bind_rows(taxa_file, taxa_sequences)
        taxa_file <- group_by(taxa_file, across(tax_level))
        taxa_file <- summarize_all(taxa_file, sum, na.rm = TRUE)
        utils::write.csv(taxa_file,
          file = file,
          row.names = FALSE
        )
      } else {
        # output.csv doesn't yet exist so need to create:
        utils::write.csv(taxa_sequences,
          file = file,
          row.names = FALSE
        )
      }
    }
    return(taxa_sequences)
  })

  taxa <- group_by(taxa, .data$Family)
  taxa <- summarize_all(taxa, mean, na.rm = TRUE)
  return(taxa)
}
