AAT_style <-
  list(
    unipolar = list(row_id_style = "min-width:300px;white-space:normal;",
                    label_widths = c(left = "0px",
                                     right = "0px"),
                    label_styles =  c(left = "",
                                      right = "")),
    bipolar = list(row_id_style = "width:0px;visibility:hidden",
                   label_widths = c(left = "100px",
                                    right = "100px"),
                   label_syles = c(left = "text-align:left;min-width:100px",
                                   right = "text-align:left;min-width:100px")),
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
                num_stimuli = 1,
                random_order = FALSE,
                audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                audio_type = "wav",
                allow_na = TRUE,
                dict = GAR::GAR_dict,
                ...) {
  #browser()
  if(!(sub_group %in% letters[1:4])){
    stop(sprintf("Unknown subgroup: %s", questionnaire))
  }

  aat <- psychTestR::join(
    lapply(1:num_stimuli, function(id){
      page_label <- sprintf("%s_%02d", label, id)
      stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                   sprintf(items_prefix_pattern, id),
                                                   audio_type))
      #browser()
      psychTestR::new_timeline(
        get_sub_group_pages(sub_group, page_label, stimulus_url, random_order, dict = dict, ...)
        , dict = dict)
    }))
  #browser()

  a <-
    psychTestR::join(
      psychTestR::begin_module(label = label),
      aat[[1]],
      # scoring
      psychTestR::end_module()
    )
  print(class(a))
  return(a)
}

bipolar_items_sets <- c("MG" = 24, "MI" = 8, "MA" = 6, "MV" = 5)
unipolar_items_sets <- c("AT" = 7, "PG" = 11)

item_sets <- list("bipolar" = bipolar_items_sets, "unipolar" = unipolar_items_sets)
all_num_items <- c(bipolar_items_sets, unipolar_items_sets)

get_sub_group_labels <- function(sub_group, sub_scale, scale_length = 5){
  if(length(sub_scale) > 1){
    sub_scale <- sub_scale[[1]]
  }
  type <- "bipolar"
  if(substr(sub_scale, 1, 1) != "M"){
    type <- "unipolar"
  }
  else{
    if(sub_group %in% c("b", "c")){
      type <- "unipolar"
    }
  }
  message(sprintf("Subscale: %s, sub_group: %s, scale: %s", sub_scale, sub_group, type))
  if(type == "bipolar"){
    label_key <- sprintf("TGAR_AAT_SD%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else {
    label_key <- sprintf("TGAR_AAT_L%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
}

get_sub_group_items <- function(sub_group, sub_scale, num_items){
  if(sub_scale[[1]] == "M"){
    #browser()
    return(get_sub_group_items(sub_group,
                               names(bipolar_items_sets),
                               all_num_items[names(bipolar_items_sets)]))
  }
  if(length(sub_scale) > 1){
    #browser()
    stopifnot(length(sub_scale) == length(num_items))
    ret <- sapply(sub_scale, function(ssc) get_sub_group_items(sub_group, ssc, num_items[[ssc]]), simplify = T, USE.NAMES = F)
    return(unlist(ret))
  }
  type <- "bipolar"
  if(substr(sub_scale, 1, 1) != "M"){
    type <- "unipolar"
  }
  else{
    if(sub_group %in% c("b", "c")){
      type <- sprintf("unipolar_%s", sub_group)
    }
  }
  if(type == "bipolar"){
    #bipolar
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else if(type == "unipolar"){
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else if(type == "unipolar_b"){
    #browser()
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) {
        ssplit <- stringr::str_split_fixed(psychTestR::i18n(sprintf(label_key, x)), "_", 2)
        ssplit[, 1]
      }, simplify = T, USE.NAMES = T)
  }
  else if(type == "unipolar_c"){
    label_key <- sprintf("TGAR_AAT_%s_%%04d_PROMPT", sub_scale)
    ret <- sapply(1:num_items, function(x) {
      ssplit <- stringr::str_split_fixed(psychTestR::i18n(sprintf(label_key, x)), "_", 2)
      ssplit[, 2]
    }, simplify = T, USE.NAMES = T)
  }
  ret
}

get_sub_group_pages <- function(sub_group, page_label, stimulus_url, random_order, dict, ...){
  preamble_key <- sprintf("TGAR_AAT_PREAMBLE")
  scale_length <- 7
  #item_key <- sprintf("TGAR_ATT_PROMPT_%%04d")
  #label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)
  #browser()
  bipolar <- psychTestR::join(
    lapply(c("M"), function(item_set)
    audio_radiobutton_matrix_page(label = sprintf("%s_%s", page_label, item_set),
                                  polarity = "bipolar",
                                  url = stimulus_url,
                                  instruction = psychTestR::i18n(preamble_key),
                                  anchors = FALSE,
                                  header = "simple_str",
                                  reduce_labels = FALSE,
                                  style = AAT_style,
                                  trigger_button_text = psychTestR::i18n("CONTINUE"),
                                  items = get_sub_group_items(sub_group, item_set, bipolar_items_sets[item_set]),
                                  choices = 0:(scale_length - 1),
                                  labels = get_sub_group_labels(sub_group, item_set, scale_length = scale_length),
                                  random_order = random_order,
                                  show_controls = TRUE,
                                  allow_download = FALSE,
                                  allow_na = TRUE,
                                  ...)))
  unipolar <- psychTestR::join(
    lapply(names(unipolar_items_sets), function(item_set){
      audio_radiobutton_matrix_page(label = sprintf("%s_%s", page_label, item_set),
                                    polarity = "unipolar",
                                    url = stimulus_url,
                                    instruction = psychTestR::i18n(preamble_key),
                                    anchors = FALSE,
                                    header = "double",
                                    reduce_labels = FALSE,
                                    style = AAT_style,
                                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                                    items = get_sub_group_items(sub_group, item_set, unipolar_items_sets[item_set]),
                                    choices = 0:(scale_length - 1),
                                    labels = get_sub_group_labels(sub_group, item_set, scale_length = scale_length),
                                    random_order = random_order,
                                    show_controls = TRUE,
                                    allow_download = FALSE,
                                    allow_na = TRUE,
                                    ...)
      }))
  #browser()
  psychTestR::join(bipolar, unipolar)
}
