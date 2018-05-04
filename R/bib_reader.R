#' Read DOIs/Metadata from bib files
#'
#' @importFrom magrittr %>%
#'
#' @param isi_dir character
#' @export
#'
#' @return data frame witht eh bibtex file content
#' @examples bibfile_reader("data/isi_search")
bibfile_reader <- function(isi_dir){
  file_list <- list.files(path = isi_dir, pattern = ".bib", full.names = TRUE) %>%
    purrr::map(bibliometrix::readFiles) %>%
    purrr::map_dfr(bibliometrix::convert2df, dbsource = "isi", format = "bibtex") %>%
    dplyr::select(TI, AU, PY, JI, SO, DI)
}
