#' Title-DOI Matcher
#'
#' This function matches titles exported from Colandr to .bib files
#' and exports the associated DOI from Web of Science
#' @param papers    A csv file exported from Colandr
#' @param file_list   A tibble of .bib files imported to Colandr
#' @param condition Condition for sorting papers
#' @keywords Colandr
#'
#' @importFrom magrittr %>%
#'
#'
#' @return A data frame of titles, journals, authors, and DOIs in the .bib format
#'
#' @examples
#' \dontrun{
#' title_to_doi(papers, df_citations, cond)
#' }
#'
title_to_doi <- function(papers, file_list, condition){

  # filter list of papers by those that are included
  # select only the relevant columns
  papers = dplyr::filter(papers, citation_screening_status == condition) %>%
    dplyr::select(citation_title,citation_authors,citation_journal_name)

  # read in bib files
  # select only relevant columns
  references = file_list

  # convert title text to lower case
  papers$citation_title <- tolower(papers$citation_title)
  references$TI <- tolower(references$TI)

  # remove extra spaces in .bib data frame
  references$TI <- gsub("\\s+", " ", stringr::str_trim(references$TI))

  # merge the two data sets
  # remove duplications
  # return tibble from the function
  references %>%
    dplyr::filter(TI %in% papers$citation_title) %>%
    unique() %>%
    return()
}
