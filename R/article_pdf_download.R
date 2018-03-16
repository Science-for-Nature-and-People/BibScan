#' Batch download
#'
#' This function processes and batch downloads files
#' @param infilepath  A character; path to target folder with input files
#' @param outfilepath A character; path to folder for export files
#' @param colandr       A file that provides titles to match; designed to be output of Colandr
#' @keywords download
#' @return
#' @export files
#' @examples
#' article_pdf_download()

article_pdf_download <- function(infilepath, outfilepath,colandr=NULL){
# ===============================
# PACKAGES
# ===============================
library(bibliometrix) # For reading and analyzing ISI data
library(tidyverse)    # For data manipulation
library(crminer)      # For getting links from DOI
library(dplyr)

# ===============================
# CONSTANTS
# ===============================
# Create the main output directory
output_dir <- file.path(outfilepath, 'output')
# Check if pdf_output directory exists
dir.create(output_dir, showWarnings = FALSE)

# PDF subdirectory
pdf_output_dir <- file.path(output_dir, 'pdfs')
# Check if pdf_output_dir directory exists
dir.create(pdf_output_dir, showWarnings = FALSE)

# Non-PDF files subdirectory
nopdf_output_dir <- file.path(output_dir, 'non-pdfs')
# Check if pdf_output_dir directory exists
dir.create(nopdf_output_dir, showWarnings = FALSE)

# ===============================
# MAIN
# ===============================
# Read .bib files
## Combine all the bib files. The user needs only add the file to the directory full of bib files
filepath_list <- as.list(file.path(infilepath, dir(infilepath)))
file_list <- convert2df(
                        do.call(
                                readFiles, filepath_list),
                        dbsource = "isi", format = "bibtex")

if(is.null(colandr) == F){
  # Read sorted list from Colandr
  papers <- read_csv(file.path(colandr))

  # Match titles from Colandr to DOIs from .bib
  matched <- title_to_doi(papers,data_dir)
}
if(is.null(colandr) == T){
    matched <- file_list
}

## STEP 1: ORGANIZE LINKS
message('===============================\nORGANIZING LINKS\n===============================')
# Select attributes of interest and clean the author field
my_df <- tibble(Name=paste(gsub(";.*$", "", matched$AU),matched$PY,matched$SO),
                DOI=matched$DI)

# Create tibble that reports information to the user
report <- my_df

# Print percent of papers with DOI
perc = suppressWarnings((nrow(filter(my_df, !is.na(DOI)))/nrow(my_df)))
suppressWarnings(perc %>%
                   "*"(100) %>%
                   round(digits=1) %>%
                   paste0("%") %>%
                   message(" of references contained a DOI"))
rm(perc)

# Add column to data frame describing if row is NA
report$DOI_exists <- ifelse(is.na(report$DOI), 'No DOI', 'DOI exists')

# Remove links with NAs
my_df <- filter(my_df, !is.na(DOI))

# Collect links
my_df$links <- sapply(my_df$DOI, crm_links)

# Count number of references that found no link
perc = 1-(nrow(my_df[lapply(my_df$links, length) == 0,])/nrow(my_df))
suppressWarnings(perc %>%
                   "*"(100) %>%
                   round(digits=1) %>%
                   paste0("%") %>%
                   message(" of references with a DOI returned a URL link"))
rm(perc)

# Add to report document which reference didn't have URL
my_df$length <- lapply(my_df$links, length)
my_df$URL_found <- ifelse(my_df$length > 0, 'URL found', 'URL not found')
report <- left_join(report,my_df) %>%
  select(Name,DOI,DOI_exists,URL_found)
my_df <- select(my_df,Name,DOI,links)

# Remove references with no URL
my_df <- my_df[lapply(my_df$links, length) > 0,]

# Elsevier links require a separate download process, so we distinguish them here
my_df$elsevier <- vector(mode="logical",length=nrow(my_df))
for (i in 1:length(my_df$links)) {
  if (grepl('elsevier',my_df$links[[i]][[1]])) { # checking for string 'elsevier' in link
    my_df$elsevier[i] <- TRUE
  } else {
    my_df$elsevier[i] <- FALSE
  }
}

# Distinguish Wiley links
my_df$wiley <- vector(mode="logical",length=nrow(my_df))
for (i in 1:length(my_df$links)) {
  if (grepl('wiley',my_df$links[[i]][[1]])) { # checking for string 'elsevier' in link
    my_df$wiley[i] <- TRUE
  } else {
    my_df$wiley[i] <- FALSE
  }
}

# Simplify list of links into single link for each DOI
for (i in 1:dim(my_df)[1]) {
  if (my_df$elsevier[i]) { # if it's from elsevier, we want to get the xml link
    link <- my_df$links[[i]]$xml$xml
  } else if (my_df$wiley[i] && is.null(my_df$links[[i]][3]$unspecified)==FALSE) { # if it's from wiley, get unidentified link
    link <- my_df$links[[i]][3]$unspecified
  } else if ('pdf' %in% names(my_df$links[[i]])) { # otherwise, we prefer the 'pdf' link type
    link <- my_df$links[[i]]$pdf$pdf
  } else if ('unspecified' %in% names(my_df$links[[i]])) { # our last preference is the 'unspecified' link type
    link <- my_df$links[[i]]$unspecified$unspecified
  } else { # we don't handle links of type 'html' or 'plain', because they almost never provide pdf download; moreover, we only want xml links from elsevier because we only handle those
    link <- NA
  }
  my_df$download_link[i] <- as.character(link)
}

# Merge final download link into report document
report <- left_join(report,my_df) %>%
  select(Name,DOI,DOI_exists,URL_found,download_link)


## STEP 2: DOWNLOAD PDFS FROM LINKS
message('===============================\nDOWNLOADING PDFS FROM LINKS\n===============================')

# elsevier_pdf_download() is called repeatedly via a loop that iterates through the rows of the dataframe
for (i in 1:nrow(my_df)) {
  url <- my_df$download_link[i]
  my_df$path[i] <- paste0(file.path(pdf_output_dir, my_df$Name[i]), '.pdf')
  if (my_df$elsevier[i]) {

    my_df$downloaded <- tryCatch(elsevier_pdf_download(url, my_df$path[i]),
                                 error=function(cond) {
                                   message(cond)
                                   return(0)
                                 },
                                 finally = message(paste("\nProcessed URL:", url)))
  } else {
    # DOWNLOADING OTHER LINKS
    my_df$downloaded[i] <- tryCatch(download.file(url, my_df$path[i]),
                                    error=function(cond) {
                                      message(cond)
                                      return(0)
                                    },
                                    finally = message(paste("\nProcessed URL:", url)))
  }
  message('[', i, '/', dim(my_df)[1], ']')
}

message('===============================\nPDFS DOWNLOADED\n===============================')

## STEP 3: POST-PROCESSING
# distinguish real pdf files from other files (mainly html webpages)
for (i in 1:dim(my_df)[1]) {
  if (file.exists(my_df$path[i])) {
    my_df$downloaded[i] <- TRUE
    my_df$is_pdf[i] <- is_binary(my_df$path[i])
  } else {
    my_df$downloaded[i] <- FALSE
    my_df$is_pdf[i] <- FALSE
  }
}
# Add the flags for downloaded and PDF to the data frame
my_df$downloaded <- as.logical(my_df$downloaded)
my_df$is_pdf <- as.logical(my_df$is_pdf)

# Extract some statistics
download_success <- sum(my_df$downloaded) # out of 5759 acquired links, 4604 produced downloaded files
unique_files <- length(unique(my_df$path[my_df$downloaded])) # out of 4604 downloaded files, 4539 are unique
unique_pdfs <- length(unique(my_df$path[my_df$downloaded & my_df$is_pdf])) # out of 4539 unique downloaded files, 4057 are binary files (PDFs)
message(sprintf("Over the %i acquired links, %i PDFs were succesfully downloaded", nrow(my_df), unique_pdfs))

# Extract the files info that were not PDFs
non_pdf_paths <- unique(my_df$path[my_df$downloaded & !my_df$is_pdf]) # For investigative purposes, here are the paths for the non-PDF files (482) that were downloaded

if(length(non_pdf_paths >0 )){
  ## Move the non-pdf files to a specific directory
  # Create the destination list
  html_paths <- file.path(
    nopdf_output_dir,
    paste0(basename(file_path_sans_ext(non_pdf_paths)),
           ".html")
  )
  # Move the files
  file.rename(from = non_pdf_paths, to = html_paths)
}

## Fix the double dot before file extension
pdf_files <- dir(pdf_output_dir, full.names = TRUE)
pdf_fixed <- gsub("\\.\\.pdf","\\.pdf",pdf_files)
file.rename(from = pdf_files , to = pdf_fixed)

# output information regarding the download processs to csv
summary_path <- file.path(output_dir, 'summary.csv')
write.csv(select(my_df, -links), file = summary_path, row.names = F)

message('\n Details of the PDF retrieval process have been stored in ', summary_path, '\n')

}
