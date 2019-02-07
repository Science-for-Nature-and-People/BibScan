library(BibScan)
article_pdf_download('Tillage_bib', 'Tillage_bib')

setwd('/Users/grant/Downloads')


summary <- read.csv('Tillage_bib/summary.csv')
View(summary)

non_pdf <- summary %>%
              filter(is_pdf == FALSE)

pdf <- summary %>%
              filter(is_pdf == TRUE)

non_pdf_journals <- gsub(".*20","", non_pdf$name)
non_pdf_journals <- substring(non_pdf_journals, 3)

pdf_journals <- gsub(".*20", "", pdf$name)
pdf_journals <- substring(pdf_journals, 3)

both <- intersect(non_pdf_journals, pdf_journals)
only_non <- setdiff(non_pdf_journals, pdf_journals)
only_pdf <- setdiff(pdf_journals, non_pdf_journals)

summary_non <- summary %>%
                  filter()

View(summary[sample(nrow(summary), 50), ])
