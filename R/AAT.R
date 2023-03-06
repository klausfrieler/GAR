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
      get_sub_group_pages(sub_group, page_label, stimulus_url)
    }))
  #browser()

  psychTestR::join(
    psychTestR::begin_module(label = label),
    aat,
    # scoring
    psychTestR::end_module()
  )
}
get_sub_group_labels <- function(sub_group, scale_length = 5){
  if(sub_group %in% c("a", "b")){
    #bipolar
    label_key <- sprintf("TGAR_SD%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else if(sub_group %in% c("b", "c")){
    label_key <- sprintf("TGAR_L%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)

  }
}
get_subgoup_pages <- function(sub_group, page_label, stimulus_url){
  preamble_key <- sprintf("TGAR_ATT_PREAMBLE")
  prompt_key <- sprintf("TGAR_ATT_PROMPT", questionnaire, num_rating_items)
  label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)

  psychTestR::new_timeline(
    audio_radiobutton_matrix_page(label = page_label,
                                  url = stimulus_url,
                                  instruction = psychTestR::i18n(preamble_key),
                                  anchors = anchors,
                                  header = TRUE,
                                  reduce_labels = TRUE,
                                  style = AAT_style,
                                  prompts = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                                  choices = 0:(scale_length-1),
                                  labels = get_subgroup_labels(sub_group),
                                  random_order = random_order,
                                  show_controls = TRUE,
                                  allow_download = FALSE,
                                  allow_na = TRUE,
                                  ...),
    dict = dict)

}
