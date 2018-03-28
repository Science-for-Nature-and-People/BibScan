#' Title-DOI Matcher
#'
#' This function matches titles exported from Colandr to .bib files
#' and exports the associated DOI from Web of Science
#' @param papers    A .csv exported from Colandr
#' @param file_list A tibble of .bib files imported to Colandr
#' @param cond      Condition for sorting papers
#' @keywords Colandr
#' @return A data frame of titles, journals, authors, and DOIs in the .bib format
#' @export
#' @examples
#' title_to_doi()

title_to_doi <- function(papers,bib.dir,condition){
  require(bibliometrix)
  require(tidyverse)

  # filter list of papers by those that are included
  # select only the relevant columns
  papers = filter(papers, citation_screening_status == condition) %>%
    select(citation_title,citation_authors,citation_journal_name)

  # read in bib files
  # select only relevant columns
  references = file_list

  # convert title text to lower case
  papers$citation_title <- tolower(papers$citation_title)
  references$TI <- tolower(references$TI)

  # remove extra spaces in .bib data frame
  references$TI <- gsub("\\s+", " ", str_trim(references$TI))

  # merge the two data sets
  # remove duplications
  # return tibble from the function
  references %>%
    filter(TI %in% papers$citation_title) %>%
    unique() %>%
    return()
}
