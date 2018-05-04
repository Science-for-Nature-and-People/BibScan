#' Tag papers from the Elsevier publisher
#'
#' @param df_doi
#' @param col_links
#'
#' @return dataframe
#' @export
#'
#' @examples elsevier_tagger(my_df, "links")
elsevier_tagger <- function(df_doi, col_links) {
  df_doi$elsevier <- as.logical(NA)
  # remove the entry without links
  df_doi <- df_doi[lapply(df_doi[ ,col_links], length) > 0, ]
  # iterate through the list looking for the word "elsevire" in the URL
  for (i in 1:nrow(df_doi)) {
    if (grepl('elsevier',df_doi[i, col_links][[1]])) { # checking for string 'elsevier' in link
      df_doi$elsevier[i] <- TRUE
    } else {
      df_doi$elsevier[i] <- FALSE
    }
  }
  return(df_doi)
}
