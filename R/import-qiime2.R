#' Import taxon data from Qiime2
#'
#' Import .csv taxonomy file export from Qiime2 Viewer. The taxonomic data is
#' re-structured to match the format required for the IQI model.
#' @param path File path to the Qiime2 Viewer exported .csv file
#' @param seed set.seed variable - default 1 to keep results reproducible. use
#'   `set.seed()` as argument if you want randomness in results.
#' @importFrom rlang .data
#' @return dataframe 682 columns with sample names as row.names.
#'
#' @export
#'
#' @examples
#' data <- import_qiime2(system.file("extdat/raw-taxa-data", "S16_TestMOWI.csv",
#'   package = "aquaman"
#' ))
import_qiime2 <- function(path = NULL, seed = 1) {
  S16Data <- readr::read_csv(path)
  SplitToTaxa <- function(S16Data) {
    message("SplitToTaxa start")
    message("Splits data into taxa levels")
    # TestNGS1=Upload16S("Test");S16Data=TestNGS1#code testing
    # TestNGS2=SplitToTaxa(TestNGS1)#Does this convert numerics to characters?
    # Run2.1=TestNGS1#code testing
    Run2.1 <- S16Data
    # change 'Unassigned..' to 'D_0__Unassigned' to aid string split code
    # colnames(Run2.1)[grep("Unassigned",colnames(Run2.1))]="D_0Unassigned"
    Run2.2 <- as.data.frame(t(Run2.1[, -1])) # transpose, don't include the sample ID, need to keep the SampleID as colnames
    # Run2.2 <- setNames(data.frame(t(Run2.1[, -1])), Run2.1[, 1]) # setNames, make the colnames=SampleID
    names(Run2.2) <- unlist(Run2.1[, 1])
    Run2.2$Taxa <- rownames(Run2.2)
    Run2.3 <- as.data.frame(Run2.2[, "Taxa"]) # isolate the taxa names, ahead of cleaning them up.
    colnames(Run2.3)[1] <- "Taxa"
    Run2.4 <- splitstackshape::cSplit(Run2.3,
      splitCols = "Taxa",
      sep = c("D_"),
      drop = FALSE
    ) # D__is the divider between taxa, from Silva
    # the cSplit inserts an NA column because the split criterion prefixes the name to be split.
    Run2.4 <- Run2.4[, -2] # delete 1st unattributed column
    colnames(Run2.4)[2:8] <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    pattern <- c("0__", "1__", "2__", "3__", "4__", "5__", "6__")
    for (i in 1:length(pattern)) {
      Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = pattern[i], replacement = "")
    } # remove the 'patterns'
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "[.]", replacement = "") # replace periods with space.The [] escapes the character
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "Ambiguous_taxa", replacement = "") # Ambig_taxa worth removing.
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "uncult", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "unknown", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "incertae", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "benzene", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "unidentified", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = ";_", replacement = NA, ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], gsub, pattern = "[^[:alnum:]]", replacement = "", ignore.case = TRUE) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], function(x) stringr::str_replace(x, ";", "")) #
    Run2.4[, 2:8] <- lapply(Run2.4[, 2:8], trimws, which = "both") # removes leading and tailing white_spaces
    # the Run2.4[] -square brackets[] keep this as a dataframe.
    # Run2.4$Taxa=Run2.4a$Taxa
    Run2.5 <- Run2.4[, 1:8]
    Run2.5 <- as.data.frame(Run2.5)
    Run2.5[is.na(Run2.5)] <- "XUnidentified"
    Run2.2$Taxa <- rownames(Run2.2)
    Run2X <- merge(Run2.5, Run2.2)
    MakeUnique <- FALSE # leave this code in.
    if (MakeUnique == TRUE) {
      id <- "XUnidentified"
      MU <- function(xxx) {
        A <- which(xxx == id)
        XY <- make.unique(xxx[A], sep = "")
        xxx[A] <- XY
        return(xxx)
      }
      Run2X[, 2:8] <- lapply(Run2X[, 2:8], MU) # removes leading and tailing white_spaces
    } # end of if(MakeUnique)
    A <- "QXQ" # tomw unique separator, chosen because unlikely to pre-exist in strings
    Run2X <- within(Run2X, {
      Phylum <- paste(Kingdom, Phylum, sep = A)
      Class <- paste(Phylum, Class, sep = A)
      Order <- paste(Class, Order, sep = A)
      Family <- paste(Order, Family, sep = A)
      Genus <- paste(Family, Genus, sep = A)
      Species <- paste(Genus, Species, sep = A)
    }) # rebuild full taxonomy
    MDS1 <- Run2X
    message("SplitToTaxa ends")
    return(MDS1)
  }

  taxa <- SplitToTaxa(S16Data)

  TaxaCollate <- function(MDS1, taxalevel) {
    message("Calling function Taxacollate")
    message("Collates data across specified taxonomic level")
    # df=as.data.table(TestNGS2);taxalevel=TaxaLevel
    df <- as.data.frame(MDS1)
    Excludes <- c("Taxa", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    Excludes2 <- setdiff(Excludes, taxalevel)
    df2 <- dplyr::select(df, -Excludes2) # df2 in data.table format hence  ..! works
    names(df2)[1] <- "Taxon"
    df2 <- data.table::as.data.table(df2)
    # df2 contains only the Taxon and the sample data.
    df3 <- reshape2::melt(df2, id = "Taxon") # rm(df3)
    df4 <- data.table::data.table(df3)
    names(df4) <- c("Taxon", "SampleID", "Total") # sensible names for summarisation step via data.table
    df4$Taxon <- as.factor(df4$Taxon)
    # browser()
    df4$Total <- as.numeric(df4$Total)
    # setDT(df4)
    # df5 <- df4[,  # this generates the summary values by taxonomic group.
    #    sum(Total)
    # ,
    # by = c(Taxon, SampleID)
    # ]
    df5 <- dplyr::group_by(df4, .data$Taxon, .data$SampleID) |>
      dplyr::summarise("Total" = sum(.data$Total))

    message("Ending function TaxaCollate")
    return(df5)
  }


  family <- TaxaCollate(taxa, "Family")
  family <- dplyr::ungroup(family)
  input <- reshape2::dcast(family, SampleID ~ Taxon, value.var = "Total")
  input[is.na(input)] <- 0
  set.seed(seed)
  input_rare <- cbind(input[, 1], vegan::rrarefy(input[, -1], 2500))
  input_rare <- data.frame(input_rare)
  # input_test <- input_rare[, names(input_rare)[names(input_rare) %in% names(aquaman::demo_taxa)]]
  # input_test <- input_rare[, c(names(input_test))]


  input_rare <- dplyr::bind_rows(aquaman::demo_taxa, input_rare)
  input_rare <- input_rare[(nrow(aquaman::demo_taxa) + 1):nrow(input_rare), ]
  input_rare[is.na(input_rare)] <- 0
  row.names(input_rare) <- input[, 1]
  return(input_rare)
}
