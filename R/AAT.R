AAT_style <- list(label_widths = list("100px", "100px"),
                  div_style = "width:60%;margin-left:20%;margin-right:20%")
#' Audio Attention Study Modules
#'
#' This function defines module for #'
#' @inheritParams audio_radiobutton_matrix_page
#'
#'
#' @export
AAT <- function(label = "AAT",
                sub_group = "a",
                items_prefix_pattern = "s%02d",
                num_stimuli = 10,
                audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                audio_type = "wav",
                dict = GAR::GAR_dict,
                ...) {
  #browser()
  if(!(sub_group %in% letters[1:4])){
    stop(sprintf("Unknown subgroup: %s", questionnaire))
  }

  num_stimuli <- 10
  aat <- psychTestR::join(
    lapply(1:num_stimuli, function(id){
      page_label <- sprintf("%s_%02d", label, id)
      stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                   sprintf(items_prefix_pattern, id),
                                                   audio_type))
      #browser()
      get_sub_group_pages(sub_group, page_label, stimulus_url, dict = dict, ...)
    }))
  #browser()

  psychTestR::join(
    psychTestR::begin_module(label = label),
    aat,
    # scoring
    psychTestR::end_module()
  )
}

get_sub_group_labels <- function(sub_group, sub_scale, scale_length = 5){
  if(sub_group %in% c("a", "b") && substr(sub_scale, 1, 1) == "M"){
    #bipolar
    label_key <- sprintf("TGAR_AAT_SD%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else if(sub_group %in% c("b", "c")){
    #unipolar
    label_key <- sprintf("TGAR_AAT_L%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
}

get_sub_group_items <- function(sub_group, sub_scale, num_items){
  if(sub_group %in% c("a", "d") || substr(sub_scale, 1, 1) == "M"){
    #bipolar
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else if(sub_group %in% c("b", "c")){
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  ret
}

get_sub_group_pages <- function(sub_group, page_label, stimulus_url, dict, ...){
  bipolar_items_sets <- c("MG" = 24, "MI" = 6, "MA" = 8, "MV" = 5)
  unipolar_items_sets <- c("AT" = 7, "PG" = 11)
  preamble_key <- sprintf("TGAR_AAT_PREAMBLE")
  scale_length <- 5
  #item_key <- sprintf("TGAR_ATT_PROMPT_%%04d")
  #label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)

  MG <-
    psychTestR::new_timeline(
    audio_radiobutton_matrix_page(label = page_label,
                                  polarity = "bipolar",
                                  url = stimulus_url,
                                  instruction = psychTestR::i18n(preamble_key),
                                  anchors = FALSE,
                                  header = "simple_num",
                                  reduce_labels = TRUE,
                                  style = AAT_style,
                                  items = get_sub_group_items(sub_group, "MG", bipolar_items_sets["MG"]),
                                  choices = 0:(scale_length - 1),
                                  labels = get_sub_group_labels(sub_group, "MG", scale_length = scale_length),
                                  random_order = random_order,
                                  show_controls = TRUE,
                                  allow_download = FALSE,
                                  allow_na = TRUE,
                                  ...),
    dict = dict)

}
