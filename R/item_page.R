get_item_page <- function(polarity,
                          page_label,
                          stimulus_url,
                          preamble_key,
                          anchors,
                          header,
                          reduce_labels,
                          style,
                          num_rating_items,
                          scale_length,
                          prompt_key,
                          label_key,
                          random_order,
                          show_controls,
                          allow_download,
                          allow_na,
                          ...){
  #browser()
  if(polarity == "unipolar"){
    item_page <- audio_radiobutton_matrix_page(label = page_label,
                                               polarity = polarity,
                                               url = stimulus_url,
                                               instruction = psychTestR::i18n(preamble_key),
                                               anchors = anchors,
                                               header = header,
                                               reduce_labels = reduce_labels,
                                               style = style,
                                               prompts = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                                               choices = 0:(scale_length - 1),
                                               labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                                               random_order = random_order,
                                               show_controls = show_controls,
                                               allow_download = allow_download,
                                               allow_na = allow_na,
                                               ...)
  }
  else{
    if(scale_length %% 2 == 0) {
      choices <- c(seq(-scale_length/2, -1), seq(1, scale_length/2))
    }
    else{
      choices <- seq(-floor(scale_length/2), floor(scale_length/2))
    }
    item_page <- audio_radiobutton_matrix_page(label = page_label,
                                               polarity = polarity,
                                               url = stimulus_url,
                                               instruction = psychTestR::i18n(preamble_key),
                                               anchors = F,
                                               header = header,
                                               reduce_labels = F,
                                               style = style,
                                               prompts = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                                               choices = choices,
                                               labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                                               random_order = random_order,
                                               show_controls = show_controls,
                                               allow_download = allow_download,
                                               allow_na = allow_na,
                                               ...)
  }

  item_page
}
