#' Read DOIs/Metadata from bib files
#'
#' @importFrom magrittr %>%
#'
#' @param isi_dir (character) Path to target folder with bibtex files
#' @export
#'
#' @return data frame with the bibtex file content
#' @examples
#' \dontrun{
#' bibfile_reader("data/isi_search")
#' }
#'
bibfile_reader <- function(isi_dir){
  file_list <- list.files(path = isi_dir, pattern = ".bib", full.names = TRUE)
  if(length(file_list) <= 0){
    stop("Check your path to the bibtex files. No files were found", call. = FALSE)
  }
  # Read the bibfiles
  biblio_data <- file_list %>%
    purrr::map(RefManageR::ReadBib) %>%
    purrr::map_dfr(as.data.frame)

  # read the lookup table to match fields
  lut <- read.csv("inst/LUT_non_isi.csv", stringsAsFactors = FALSE)

  # Check if the necessary fields are here
  stopifnot(sum(lut$dillon %in% names(biblio_data)) == 5)

  # Select the relevant fields
  biblio_data <- biblio_data[, lut$dillon]

  # Match the fields names handling potential order discrepency to rename fields according to web of science tags
  names(biblio_data) <- lut$WoS_tag[match(names(biblio_data), lut$dillon)]

  # remove the curly braces aroung the titles
  biblio_data$TI <- gsub("\\{|\\}", "", biblio_data$TI)

  return(biblio_data)
}
