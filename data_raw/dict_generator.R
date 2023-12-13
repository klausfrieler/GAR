library(tidyverse)
GAR_dict_raw <- read.csv(fileEncoding = "UTF8",
                     file = "./data_raw/GAR_dict.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE) %>% as_tibble()

GAR_dict_raw_AAT <- read.csv(fileEncoding = "UTF8",
                            file = "./data_raw/GAR_dict_AAT.csv",
                            stringsAsFactors = FALSE,
                            header = TRUE) %>% as_tibble()

GAR_dict_raw_MAS <- read.csv(fileEncoding = "UTF8",
                             file = "./data_raw/GAR_dict_MAS.csv",
                             stringsAsFactors = FALSE,
                             sep = ";",
                             header = TRUE) %>% as_tibble()

if(!("de" %in% names(GAR_dict_raw))){
  GAR_dict_raw$de <- GAR_dict_raw$de_f
}

if(!("de" %in% names(GAR_dict_raw))){
  GAR_dict_raw_AAT$de <- GAR_dict_raw_AAT$de_f
}

if(!("de" %in% names(GAR_dict_raw_MAS))){
  GAR_dict_raw_MAS$de <- GAR_dict_raw_MAS$de_f
}

GAR_dict <- psychTestR::i18n_dict$new(
  bind_rows(GAR_dict_raw,
            GAR_dict_raw_AAT,
            GAR_dict_raw_MAS) %>%
    distinct() %>%
    mutate(across(where(is.character), trimws)))

usethis::use_data(GAR_dict, overwrite = TRUE)
