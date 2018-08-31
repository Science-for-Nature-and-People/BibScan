#' Read DOIs/Metadata from bib files
#'
#' @importFrom magrittr %>%
#'
#' @param isi_dir (character) Path to target folder with bibtex files
#' @param formatting (character) format used by the bibtex files
#'
#' @export
#'
#' @importFrom purrr map map_dfr
#' @importFrom tibble as_tibble
#' @importFrom readr read_csv
#'
#' @return data frame with the bibtex file content
#'
#' @examples
#' \dontrun{
#' bibfile_reader("data/isi_search")
#' }
#'
bibfile_reader <- function(isi_dir, formatting){
  file_list <- list.files(path = isi_dir, pattern = ".bib", full.names = TRUE)
  if(length(file_list) <= 0){
    stop("Check your path to the bibtex files. No files were found", call. = FALSE)
  }
  # Read the bibfiles
  biblio_data <- file_list %>%
    purrr::map(RefManageR::ReadBib) %>%
    purrr::map_dfr(as.data.frame)

  # read the lookup table to match fields
  lut <- readr::read_csv("inst/LUT_non_isi.csv")
  # Check if the necessary fields are here
  stopifnot(sum(lut[[formatting]] %in% names(biblio_data)) == 5)

  # Select the relevant fields
  biblio_data <- biblio_data[, lut[[formatting]]]

  # Match the fields names handling potential order discrepency to rename fields according to web of science tags
  names(biblio_data) <- lut[match(names(biblio_data), lut[[formatting]]),][["WoS_tag"]]

  # remove the curly braces around values
  biblio_data <- tibble::as_tibble(map(biblio_data, ~gsub("\\{|\\}", "", .x), biblio_data))

  return(biblio_data)
}
