library(tidyverse)
GAR_dict_raw <- read.csv(fileEncoding = "UTF8",
                     file = "./data_raw/GAR_dict.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE) %>% as_tibble()

GAR_dict_raw_AAT <- read.csv(fileEncoding = "UTF8",
                            file = "./data_raw/GAR_dict_AAT.csv",
                            stringsAsFactors = FALSE,
                            header = TRUE) %>% as_tibble()

GAR_dict <- psychTestR::i18n_dict$new(bind_rows(GAR_dict_raw, GAR_dict_raw_AAT) %>% distinct())

usethis::use_data(GAR_dict, overwrite = TRUE)
