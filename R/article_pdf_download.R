#' Batch download articles from bibtex files
#'
#' @importFrom magrittr %>%
#'
#' @param infilepath  (character) path to target folder with input files
#' @param outfilepath (character) path to folder for export files
#' @param colandr     A file (character) that provides titles to match; designed to be output of Colandr
#' @param cond        Condition (logical) that defines sorting of output from Colandr file
#'
#' @return data frame containing dowload information
#' @export
#' @examples \dontrun{ article_pdf_download(infilepath = "/data/isi_searches", outfilepath = "data")}
article_pdf_download <- function(infilepath, outfilepath = infilepath, colandr=NULL, cond="included"){
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
  df_citations <- bibfile_reader(infilepath)

  # Join the DOI to Colandr output (https://www.colandrcommunity.com)
  if(is.null(colandr) == F){
    # Read sorted list from Colandr
    papers <- readr::read_csv(file.path(colandr))

    # Match titles from Colandr to DOIs from .bib
    matched <- title_to_doi(papers,df_citations,cond)

  }else{
    matched <- df_citations
  }

  ## STEP 1: ORGANIZE LINKS
  message('===============================\nORGANIZING LINKS\n===============================')
  # Select attributes of interest and clean the author field
  my_df <- tibble::tibble(Name=paste(gsub(";.*$", "", matched$AU),matched$PY,matched$SO),
                          DOI=matched$DI)

  # Create tibble that reports information to the user
  report <- my_df

  # Print percent of papers with DOI
  perc = suppressWarnings((nrow(dplyr::filter(my_df, !is.na(DOI)))/nrow(my_df)))
  suppressWarnings(perc %>%
                     "*"(100) %>%
                     round(digits=1) %>%
                     paste0("%") %>%
                     message(" of references contained a DOI"))
  rm(perc)

  # Add column to data frame describing if DOI is NA
  report$DOI_exists <- ifelse(is.na(report$DOI), FALSE, TRUE)

  # Remove links with NAs
  my_df <- dplyr::filter(my_df, !is.na(DOI))

  # Collect links
  my_df$links <- sapply(my_df$DOI, crminer::crm_links)

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
  my_df$URL_found <- ifelse(my_df$length > 0, TRUE, FALSE)
  report <- dplyr::left_join(report,my_df, by = c("Name", "DOI")) %>%
    dplyr::select(Name,DOI,DOI_exists,URL_found)
  my_df <- dplyr::select(my_df,Name,DOI,links)

  # Remove references with no URL
  my_df <- my_df[lapply(my_df$links, length) > 0,]

  # Elsevier links require a separate download process, so we distinguish them here
  my_df <- elsevier_tagger(my_df, "links")

  ## STEP 2: DOWNLOAD PDFS FROM LINKS
  message('===============================\nDOWNLOADING PDFS FROM LINKS\n===============================')

  #initialize the column to store the PDF filename
  my_df$downloaded_file <- as.character(NA)

  ## Download the PDFs
  # Set the cache path
  crminer::crm_cache$cache_path_set(path = pdf_output_dir, type = "function() getwd()")
  # Clear the cache
  crminer::crm_cache$delete_all()
  nb_pdfs <- length(crminer::crm_cache$list())
  # Download the PDFs
  for (i in 1:nrow(my_df)) {
    message(sprintf("number of papers downloaded %i",nb_pdfs))
    # my_df$path[i] <- paste0(file.path(pdf_output_dir, my_df$Name[i]), '.pdf')
    tryCatch(crminer::crm_text(my_df$links[[i]], type = "pdf", cache=FALSE, overwrite_unspecified=TRUE),
             my_df$downloaded_file[i] <- crminer::crm_cache$list()[i],
             # (url, my_df$path[i], overwrite_unspecified = TRUE),
             error=function(cond) {
               # we don't handle links of type 'html' or 'plain', because they almost never provide pdf download; moreover, we only want xml links from elsevier because we only handle those
               # link <- NA
               message(sprintf("There was a problem downloading this link %s", my_df$links[[i]]))
               # message(cond)
             },
             finally = message(sprintf("\nThe reference %s has been processed \n", my_df$Name[[i]]))
             )
    # keep track of the PDF names
    if (length(crminer::crm_cache$list()) > nb_pdfs) {
      nb_pdfs <- length(crminer::crm_cache$list())
      my_df$downloaded_file[i] <- crminer::crm_cache$list()[nb_pdfs]
    } else {
      my_df$downloaded_file[i] <- NA
    }
  }


  message('===============================\nPDFS DOWNLOADED\n===============================')

  ## STEP 3: POST-PROCESSING
  # distinguish real pdf files from other files (mainly html webpages)

  my_df$downloaded <- as.logical(NA)
  my_df$is_pdf <- as.logical(NA)

  for (i in 1:dim(my_df)[1]) {
    if (file.exists(my_df$downloaded_file[i])) {
      my_df$downloaded[i] <- TRUE
      my_df$is_pdf[i] <- is_binary(my_df$downloaded_file[i])
    } else {
      my_df$downloaded[i] <- FALSE
      my_df$is_pdf[i] <- FALSE
    }
  }
  # Add the flags for downloaded and PDF to the data frame
  # my_df$downloaded <- as.logical(my_df$downloaded)
  # my_df$is_pdf <- as.logical(my_df$is_pdf)

  # Extract some statistics
  download_success <- sum(my_df$downloaded, na.rm = TRUE) # out of 5759 acquired links, 4604 produced downloaded files
  unique_files <- length(unique(my_df$downloaded_file[my_df$downloaded])) # out of 4604 downloaded files, 4539 are unique
  unique_pdfs <- length(unique(my_df$downloaded_file[my_df$downloaded & my_df$is_pdf])) # out of 4539 unique downloaded files, 4057 are binary files (PDFs)
  message(sprintf("Over the %i acquired links, %i PDFs were succesfully downloaded", nrow(my_df), unique_pdfs))

  # Extract the files info that were not PDFs
  non_pdf_paths <- unique(my_df$downloaded_file[my_df$downloaded & !my_df$is_pdf]) # For investigative purposes, here are the paths for the non-PDF files (482) that were downloaded

  if(length(non_pdf_paths > 0)){
    ## Move the non-pdf files to a specific directory
    # Create the destination list
    html_paths <- file.path(
      nopdf_output_dir,
      paste0(basename(tools::file_path_sans_ext(non_pdf_paths)),
             ".html")
    )
    # Move the files
    file.rename(from = non_pdf_paths, to = html_paths)
  }

  # ## Fix the double dot before file extension
  # pdf_files <- dir(pdf_output_dir, full.names = TRUE)
  # pdf_fixed <- gsub("\\.\\.pdf","\\.pdf",pdf_files)
  # file.rename(from = pdf_files , to = pdf_fixed)

  # output information regarding the download processs to csv
  summary_path <- file.path(output_dir, 'summary.csv')
  write.csv(dplyr::select(my_df, -links), file = summary_path, row.names = F)

  message('\n Details of the PDF retrieval process have been stored in ', summary_path, '\n')

  return(my_df)
}
