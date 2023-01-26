GAR_dict_raw <- read.csv(fileEncoding = "UTF8",
                     file = "./data_raw/GAR_dict.csv",
                     stringsAsFactors = FALSE,
                     header = TRUE)

GAR_dict <- psychTestR::i18n_dict$new(GAR_dict_raw)

usethis::use_data(GAR_dict, overwrite = TRUE)
