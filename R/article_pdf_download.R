#' Batch download articles from bibtex files
#'
#'
#' @param infilepath  (character) path to target folder with input files
#' @param bib_format  (character) format used by the bibtex file
#' @param outfilepath (character) path to folder for export files
#' @param colandr     A file (character) that provides titles to match; designed to be output of Colandr
#' @param cond        Condition (logical) that defines sorting of output from Colandr file
#'
#' @return data frame containing dowload information
#'
#' @importFrom magrittr %>%
#' @importFrom readr read_csv write_csv
#'
#' @export
#' @examples \dontrun{ article_pdf_download(infilepath = "/data/isi_searches", outfilepath = "data")}

article_pdf_download <- function(infilepath, bib_format, outfilepath = infilepath, colandr=NULL, cond="included"){
  # ===============================
  # CONSTANTS
  # ===============================
  # Create the main output directory
  dir.create(outfilepath, showWarnings = FALSE)

  # PDF subdirectory
  pdf_output_dir <- file.path(outfilepath, 'pdfs')

  # Non-PDF files subdirectory
  nopdf_output_dir <- file.path(outfilepath, 'non-pdfs')


  # ===============================
  # MAIN
  # ===============================
  # Read .bib files
  df_citations <- bibfile_reader(infilepath, bib_format)

  # Join the DOI to Colandr output (https://www.colandrcommunity.com)
  if(is.null(colandr) == F){

    # Read sorted list from Colandr
    papers <- readr::read_csv(file.path(colandr))

    # Match titles from Colandr to DOIs from .bib
    matched <- title_to_doi(papers, df_citations, cond)

  }else{
    #just duplicate
    matched <- df_citations
  }

  ## STEP 1: ORGANIZE LINKS
  message('===============================\nORGANIZING LINKS\n===============================')
  # Select attributes of interest and clean the author field
  my_df <- tibble::tibble(name = paste(gsub(";.*$", "", matched$AU),matched$PY,matched$SO),
                          titles = matched$TI,
                          DOI = matched$DI)

  # Print percent of papers with DOI
  perc <- suppressWarnings((nrow(dplyr::filter(my_df, !is.na(DOI)))/nrow(my_df)))
  suppressWarnings(perc %>%
                     "*"(100) %>%
                     round(digits=1) %>%
                     paste0("%") %>%
                     message(" of references contained a DOI"))
  rm(perc)


  # Create tibble that reports information to the user keeping all the original entries
  report <- my_df

  # Add column to data frame describing if DOI is NA
  report$DOI_exists <- !(is.na(report$DOI))

  # Remove links with NAs
  my_df <- dplyr::filter(my_df, !is.na(DOI))

  # Collect URLs via CrossRef API (takes time to run)
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
  report <- dplyr::left_join(report, my_df, by = c("name", "DOI", "titles"))
  report$length <- lapply(report$links, length)
  report$URL_found <- ifelse((report$length > 0), TRUE, FALSE)
  report <- dplyr::select(report, name, titles, DOI, DOI_exists, URL_found)

  my_df <- dplyr::select(my_df, name, titles, DOI, links)

  # Remove references with no URL
  my_df <- my_df[lapply(my_df$links, length) > 0, ]

  # Elsevier links require a separate download process, so we distinguish them here
  # my_df <- elsevier_tagger(my_df, "links") not neede anymore

  ## STEP 2: DOWNLOAD PDFS FROM LINKS
  message('===============================\nDOWNLOADING PDFS FROM LINKS\n===============================')

  ## Download the PDFs
  # Set the cache path
  full_outpath <<- dirname(outfilepath) #fix to be discussed with the crminer maintainer
  crminer::crm_cache$cache_path_set(path = "", type = "function() full_outpath", prefix=basename(outfilepath))
  # crminer::crm_cache$cache_path_set(path = "soil_pdfs", type = "function() '/Users/brun'", prefix = "Desktop")

  # initialize the column to store the PDF filename
  my_df$downloaded_file <- as.character(NA)

  # Clear the cache
  crminer::crm_cache$delete_all()
  nb_pdfs <- length(crminer::crm_cache$list())
  old_cache <- crminer::crm_cache$delete_all()

  # Download the PDFs
  for (i in 1:nrow(my_df)) {
    message(sprintf("number of papers downloaded %i",nb_pdfs))
    # my_df$path[i] <- paste0(file.path(pdf_output_dir, my_df$name[i]), '.pdf')
    tryCatch(crminer::crm_text(my_df$links[[i]], type = "pdf", cache=FALSE, overwrite_unspecified=TRUE),
             # my_df$downloaded_file[i] <- crminer::crm_cache$list()[i],
             # (url, my_df$path[i], overwrite_unspecified = TRUE),
             error=function(cond) {
               # we don't handle links of type 'html' or 'plain', because they almost never provide pdf download; moreover, we only want xml links from elsevier because we only handle those
               # link <- NA
               message(sprintf("There was a problem downloading this link %s", my_df$links[[i]]))
               # message(cond)
             },
             finally = message(sprintf("\nThe reference %s has been processed \n", my_df$name[[i]]))
             )
    # keep track of the PDF names
    if (length(crminer::crm_cache$list()) > nb_pdfs) {
      # print(crminer::crm_cache$list())
      nb_pdfs <- length(crminer::crm_cache$list())
      last_paper <- setdiff(crminer::crm_cache$list(), old_cache)
      my_df$downloaded_file[i] <- last_paper
    } else {
      my_df$downloaded_file[i] <- NA
    }
    old_cache <- crminer::crm_cache$list()
  }


  message('===============================\nPDFS DOWNLOADED\n===============================')

  ## STEP 3: POST-PROCESSING
  # distinguish real pdf files from other files (mainly html webpages)

  # Check if pdf_output directory exists
  # dir.create(output_dir, showWarnings = FALSE)
  # Check if pdf_output_dir directory exists
  # dir.create(pdf_output_dir, showWarnings = FALSE)
  # Check if pdf_output_dir directory exists
  dir.create(nopdf_output_dir, showWarnings = FALSE)

  report <- my_df %>%
    select(DOI, downloaded_file) %>%
    dplyr::right_join(., report, by = "DOI")

  report$downloaded <- as.logical(NA)
  report$is_pdf <- as.logical(NA)

  for (i in 1:nrow(report)) {
    print(report$downloaded_file[i])
    if (file.exists(report$downloaded_file[i])) {
      # test if the file has been downloaded
      report$downloaded[i] <- TRUE
      # test if the file is binary (assumed to be PDF) or not (html or other)
      report$is_pdf[i] <- suppressWarnings(is_binary(report$downloaded_file[i]))
    } else {
      report$downloaded[i] <- FALSE
      report$is_pdf[i] <- FALSE
    }
  }


  # Extract some statistics
  download_success <- sum(report$downloaded, na.rm = TRUE) # out of 5759 acquired links, 4604 produced downloaded files
  unique_files <- length(unique(report$downloaded_file[report$downloaded])) # out of 4604 downloaded files, 4539 are unique
  unique_pdfs <- length(unique(report$downloaded_file[report$downloaded & report$is_pdf])) # out of 4539 unique downloaded files, 4057 are binary files (PDFs)
  message(sprintf("Over the %i acquired links, %i PDFs were succesfully downloaded", nrow(report), unique_pdfs))

  # Extract the files info that were not PDFs
  non_pdf_paths <- unique(report$downloaded_file[report$downloaded & !report$is_pdf]) # For investigative purposes, here are the paths for the non-PDF files (482) that were downloaded

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

  # TO DOs: Add file renaming

  # output information regarding the download processs to csv
  summary_path <- file.path(outfilepath, 'summary.csv')
  readr::write_csv(report, path = summary_path)

  message('\n Details of the PDF retrieval process have been stored in ', summary_path, '\n')

  return(report)
}
