# Trying to read this PDF

pdfpath <- "https://www.census.gov/content/dam/Census/library/publications/2011/dec/c2010br-08.pdf"

library(pdftools)
library(tidyverse)

PDFInfo <- pdftools::pdf_info(pdfpath)
txt <- pdftools::pdf_text(pdfpath)

page2 <- txt[[2]]

# page2tibble <- tibble(Text=page2)
page2split <- str_split(page2, "\\n", simplify = TRUE) %>% as.vector()

page2tibble <- tibble(RawText=page2split) %>%
  filter(row_number()>=13) %>%
  filter(row_number()<=50) %>%
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)+", "\\.")) %>%
  separate(TextSquish, into=c("State", "Rest"), sep="\\.", extra="merge") %>%
  mutate(Rest=str_squish(Rest),
         Rest=str_replace_all(Rest, "\\(X\\)", "NA")) %>%
  separate(Rest,
           into=c("TotalPop", "ResPop", "OverseasPop", str_c("Rep", c(2020-10*1:11))),
           sep="\\s", convert=TRUE) %>%
  select(-RawText) %>%
  mutate(TotalPop=parse_number(TotalPop),
         ResPop=parse_number(ResPop),
         OverseasPop=parse_number(OverseasPop))

glimpse(page2tibble)

page2tibble

write_rds(page2tibble, "DataOut/Census2010Apportionment.rds")  
