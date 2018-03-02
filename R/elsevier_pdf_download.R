#' Elsevier check
#'
#' This function uses webscraping techniques to download a PDF file from elsevier's server
#' @param elsevier_xml_link A character; elsevier link of type xml as returned by crminer's crmlinks() function
#' @param filepath A character; path to desired output location for downloaded PDF
#' @keywords elsevier
#' @return Nothing. Download PDFs to HD
#' @export
#' @examples
#' elsevier_pdf_download('https://api.elsevier.com/content/article/PII:0167198795004585?httpAccept=text/xml','/Users/timothy/Documents/soilc-text_mapping/data')

elsevier_pdf_download <- function(elsevier_xml_link, filepath) {
  if (!'xml2' %in% installed.packages()) install.packages("xml2")
  library(xml2)

  # getting science direct link from elsevier xml contents
  scidir_html_link <- elsevier_xml_link %>%
    read_xml() %>%
    xml_find_all('//@href') %>%
    xml_text()

  # getting route to initial pdf download link on science direct's domain
  json_obj <- scidir_html_link[2] %>% # we need to index to the second link in the `scidir_html_link` obj
    read_html() %>% # get html response from science direct link retrieved from elsevier XML response
    html_nodes('body') %>% # start from the body section
    html_nodes('div') %>% # get the div contents within that body section
    html_nodes('script') %>% # get the script contents within that div
    html_text() %>% # extract text (JSON string)
    fromJSON() # convert JSON string to R obj

  # constructing url; second arg in paste function is route to initial pdf link obtained by navigating R obj representation of the JSON string
  url_of_interest <- paste0('https://www.sciencedirect.com', json_obj$article$pdfDownload$linkToPdf)

  # getting temporary link
  read_html(url_of_interest) %>%
    html_node('head') %>% # start in the head section of the html
    html_nodes('meta[http-equiv="Refresh"]') %>% # from there, seek a meta tag with attribute http-equiv="Refresh"
    html_attr('content') %>% # within that tag, grab the character string containing the PDF direct access url
    substr(7, 100000000L) %>% # the first 6 chars are not part of the url we want, so we slice them off
    download.file(filepath) # download pdf to provided filepath
}
