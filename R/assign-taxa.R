#' Assign Taxa
#'
#' WORK IN PROGRESS - DO NOT USE IN PRODUCTION - Assign family taxa from .fastq
#' files. Will prompt you to download taxonomic reference file on first use.
#'
#' @param path to folder containing folder(s) of fastq files. Assuming
#'   DNA foldernames contain `001` and filenames have format
#'   `SAMPLENAME_XXX.fastq` - as per normal Illumina naming.
#' @param multithread Set to FALSE on windows. Only applies to
#'   dada2::filterAndTrim
#'   function `https://github.com/benjjneb/dada2/issues/1100`. The rest of the
#'   DADA2 functions will use multithread even on windows.
#' @param tax_level The taxonomic levels being assigned. Default is  "family",
#'   options are c("kingdom", "phylum", "class", "order", "family", "genus",
#'   "species").
#' @return dataframe containing assigned fmaily level taxa
#' @export
#' @importFrom dada2 filterAndTrim learnErrors dada mergePairs makeSequenceTable getSequences removeBimeraDenovo assignTaxonomy plotErrors getUniques
#' @importFrom dplyr select everything rename starts_with
#' @importFrom purrr map_df
#' @importFrom utils head capture.output
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' taxa <- assign_taxa(path = demo_path())
#' }
#'
assign_taxa <- function(path = NULL, tax_level = "family", multithread = FALSE) {
  warning("WORK IN PROGRESS - DO NOT USE IN PRODUCTION")
  set.seed(100) # Initialize random number generator for reproducibility
  # Read files ---------------------------------------------------
  # List all directories in path
  dirs <- sort(list.dirs(path, full.names = TRUE))
  # Only want folder containing DNA - should be '001'
  dna_dirs <- dirs[grep("001", dirs)]
  # Forward and reverse fastq filenames have format:
  # SAMPLENAME_R1_001.fastq
  # SAMPLENAME_R2_001.fastq
  # Loop through files ------------------------------------------
  taxa <- map_df(dna_dirs, function(dir) {
    fn_fs <- sort(list.files(dir, pattern = "_R1_001.fastq", full.names = TRUE))
    fn_rs <- sort(list.files(dir, pattern = "_R2_001.fastq", full.names = TRUE))

    # Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
    sample_names <- sapply(strsplit(basename(fn_fs), "_"), `[`, 1)

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
    # nchar("CTACGGGNGGCWGCAGCCTACGGGNGGCWGCAG")
    # nchar("GACTACHVGGGTATCTAATCCGACTACHVGGGTATCTAATCC")
    out <- filterAndTrim(fn_fs,
      filt_fs,
      fn_rs,
      filt_rs,
      truncLen = c(230, 225),
      maxN = 0,
      maxEE = c(8, 8),
      truncQ = 2,
      rm.phix = TRUE,
      compress = TRUE,
      trimLeft = c(33, 42),
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

    get_n <- function(x) sum(getUniques(x))
    track <- cbind(
      out,
      get_n(dada_fs),
      get_n(dada_rs),
      get_n(mergers),
      rowSums(seq_table_nochim)
    )

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
    taxa_file <- file_download()

    taxa <- assignTaxonomy(seq_table_nochim,
      taxa_file,
      multithread = TRUE
    )
    # Merge number of reads and taxa name in single table
    sequences <- data.frame("reads" = t(seq_table_nochim))
    taxa_sequences <- merge(taxa, sequences, by = 0)
    # Return sample_id so easy to join to sample meta data
    taxa_sequences$sample_id <- sample_names
    taxa_sequences <- select(taxa_sequences, -.data$Row.names)
    message(paste(capture.output(head(taxa_sequences, 4)), collapse = "\n"))
    # Only want column that matches taxa_level e.g. "Family"
    name <- names(taxa_sequences)[grep(tax_level,
      names(taxa_sequences),
      ignore.case = TRUE
    )]
    taxa_sequences <- select(
      taxa_sequences,
      .data$sample_id,
      starts_with(name),
      .data$reads
    )
  })
  return(taxa)
}
