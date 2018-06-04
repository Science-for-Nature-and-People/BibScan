#' Read DOIs/Metadata from bib files
#'
#' @importFrom magrittr %>%
#'
#' @param isi_dir (character) Path to target folder with bibtex files
#' @export
#'
#' @return data frame witht eh bibtex file content
#' @examples \dontrun{bibfile_reader("data/isi_search")}

bibfile_reader <- function(isi_dir){
  file_list <- list.files(path = isi_dir, pattern = ".bib", full.names = TRUE)
  if(length(file_list) <= 0){
    stop("Check your path to the bibtex files. No files were found", call. = FALSE)
  }
  biblio_data <- file_list %>%
    purrr::map(bibliometrix::readFiles) %>%
    purrr::map_dfr(bibliometrix::convert2df, dbsource = "isi", format = "bibtex") %>%
    dplyr::select(TI, AU, PY, JI, SO, DI)
}
