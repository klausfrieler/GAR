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

get_AAT_stimulus_order <- function(num_stimuli){
  function(state, ...){
    seed <-  psychTestR::get_session_info(state, complete = F)$p_id %>%
      digest::sha1() %>%
      charToRaw() %>%
      as.integer() %>%
      sum()
    set.seed(seed)
    item_order <-  sample(1:num_stimuli)
    messagef("[AAT item_oder] p_id: %s, seed:%d, order: [%s]",
             psychTestR::get_session_info(state, complete = F)$p_id,
             seed, paste(item_order, collapse = " "))
    item_order
  }
}


#' Audio Attention Study Modules
#'
#' This function defines module for #'
#' @inheritParams audio_radiobutton_matrix_page
#' @param sub_group (character scalar, "a", "b", "c" or "d")
#' One of four subgroups (bipolar/unipolar, switched labels)
#' @param item_prefix_pattern (format string)
#' Used for identifying items using audio_url and audio_type
#' @param num_stimuli (integer)
#' Number of stimuli
#' @param random_order (boolean)
#' Randomize items (not stimuli!)
#' @param with_module (boolean)
#' Wrap AAt into module.
#' @param header_style (string)
#' Header style to be pass on to make_ui_radiobutton_NAFC
#' @param audio_url (URL)
#' Where are the stimuli?
#' @param audio_type (character scalar)
#' Audio type of stimuli (as file extension)
#' @param allow_na (boolean, integer)
#' Indicates whether unanswered radio buttons are allowed, if an integer then this is the number of NAs can be tolerated.
#' @param dict (psychTestR dictionary object)
#' You really want another dictionary?
#' @export
AAT <- function(label = "AAT",
                sub_group = "a",
                items_prefix_pattern = "s%03d",
                num_stimuli = 1,
                item_order = NULL,
                randomize_stimuli = FALSE,
                random_order = FALSE,
                with_module = F,
                header_style = NULL,
                style = AAT_style,
                audio_url = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/AAT",
                audio_type = "wav",
                allow_na = c("bipolar" = 2, "unipolar" = FALSE),
                dict = GAR::GAR_dict,
                ...) {
  if(!(sub_group %in% letters[1:4])){
    stop(sprintf("Unknown subgroup: %s", sub_group))
  }
  if(length(allow_na) == 0 || length(allow_na) > 2){
    stop(sprintf("allow_na must be logical/integer of length 1 or 2"))
  }
  if(length(allow_na)  == 2){
    names(allow_na) <- c("bipolar", "unipolar")
  }
  if(length(allow_na)  == 1){
    allow_na <- c("bipolar" = allow_na, "unipolar" = allow_na)
  }
  aat_pages <-
    lapply(1:num_stimuli, function(id){
      page_label <- sprintf("%s_%03d", label, id)
      stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                   sprintf(items_prefix_pattern, id),
                                                   audio_type))
      #browser()
      psychTestR::new_timeline(
        get_sub_group_pages(sub_group,
                            page_label,
                            stimulus_url,
                            random_order,
                            id,
                            num_stimuli,
                            style = style,
                            header_style = header_style,
                            allow_na = allow_na,
                            ...),
        dict = dict)
      })

  #aat <- do.call(psychTestR::join, aat)

  save_stimuli <- function(label){
    function(order, state, ...){
      #browser()
      if(!is.null(item_order)){
        stimuli <- sprintf("%s", item_order[1:num_stimuli])[order]
      }
      else{
        stimuli <- sprintf(items_prefix_pattern, 1:num_stimuli)[order]
      }
      message(sprintf("Saving stimulus order for '%s' (length: %d): %s", label,
                      length(order),
                      paste(stimuli, collapse = ", ")))
      psychTestR::save_result(state, label, stimuli)
    }}
  if(randomize_stimuli){
    psychTestR::join(
      psychTestR::begin_module(label = label),
      psychTestR::randomise_at_run_time(label,
                                    logic = aat_pages,
                                    #get_order = get_AAT_stimulus_order(num_stimuli),
                                    save_order = save_stimuli("AAT_stim_order_random")),
      psychTestR::end_module()
    )
  }
  else{
    psychTestR::join(
      psychTestR::begin_module(label = label),
      psychTestR::order_at_run_time(label,
                                    logic = aat_pages,
                                    get_order = function(...) 1:num_stimuli,
                                    save_order = save_stimuli("AAT_stim_order_fixed")),
      # scoring
      psychTestR::end_module()
    )

  }

  # a <-
  #   psychTestR::join(
  #     if(with_module)psychTestR::begin_module(label = label),
  #     if(randomize_stimuli) psychTestR::randomise_at_run_time(label,
  #                                                             logic = aat,
  #                                                             save_order = save_stimuli("stimulus_order")),
  #
  #     if(!randomize_stimuli) psychTestR::order_at_run_time(label,
  #                                                          logic = aat,
  #                                                          get_order = function(...) 1:num_stimuli,
  #                                                          save_order = save_stimuli("stimulus_order")),
  #     if(with_module)psychTestR::end_module()
  #   )
  # #print(class(a))
  # return(a)
}

bipolar_items_sets <- c("MG" = 14, "MI" = 6, "MA" = 6, "MV" = 4)
unipolar_items_sets <- c("AT" = 5, "PG" = 10)

item_sets <- list("bipolar" = bipolar_items_sets,
                  "unipolar" = unipolar_items_sets)
all_num_items <- c(bipolar_items_sets,
                   unipolar_items_sets)

get_sub_group_labels <- function(sub_group, sub_scale, scale_length = 5){
  if(length(sub_scale) > 1){
    sub_scale <- sub_scale[[1]]
  }
  # type <- "bipolar"
  # if(substr(sub_scale, 1, 1) != "M" ){
  #   type <- "unipolar"
  # }
  # else{
  #   if(sub_group %in% c("b", "c")){
  #     type <- "unipolar"
  #   }
  # }
  type <- "unipolar"
  if(sub_group == "a" || sub_group == "d") type <- "bipolar"
  #message(sprintf("Subscale: %s, sub_group: %s, scale: %s", sub_scale, sub_group, type))
  if(type == "bipolar"){
    label_key <- sprintf("TGAR_AAT_SD%s_CHOICE%%01d", scale_length)
    #label_key <- sprintf("TGAR_MAS%s_CHOICE%%01d", scale_length)
    sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T)
  }
  else {
    label_key <- sprintf("TGAR_MAS%s_CHOICE%%01d", scale_length)
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
    ret <- sapply(sub_scale,
                  function(ssc) get_sub_group_items(sub_group, ssc, num_items[[ssc]]),
                  simplify = T, USE.NAMES = F)
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

get_sublabels <- function(type, scale_length){
  if(type == "none") {
    sublabels <- rep("", scale_length)
  }
  else if(type == "directed"){
    sublabels <- 1:scale_length
  }
  else if(type == "symmetric"){
    if(scale_length %% 2 == 0) {
      sublabels <- abs(c(seq(-scale_length/2, -1), seq(1, scale_length/2)))
    }
    else{
      sublabels <- abs(seq(-floor(scale_length/2), floor(scale_length/2)))
    }
  }
  else {
    stop(Sprintf("Unknown sublabel type: %s", type))
  }
  sublabels
}

get_sub_group_pages <- function(sub_group,
                                page_label,
                                stimulus_url,
                                random_order,
                                item_id,
                                num_stimuli,
                                style = AAT_style,
                                response_scale = "L7",
                                header_style = NULL,
                                allow_na = c("bipolar" = 2, "unipolar" = FALSE),
                                ...){
  preamble_key <- sprintf("TGAR_AAT_PREAMBLE")
  scale_length <- 7

  #item_key <- sprintf("TGAR_ATT_PROMPT_%%04d")
  #label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)
  #browser()
  #message(sprintf("Subgroup: %s, sublabel type: %s", sub_group, c(a = "directed", b = "directed", c = "directed", d = "symmetric")[sub_group]))
  sublabel_type <- c(a = "symmetric", b = "directed", c = "directed", d = "symmetric")[sub_group]
  bipolar <- psychTestR::join(
    lapply(c("M"), function(item_set)
    audio_radiobutton_matrix_page(label = sprintf("%s_%s", page_label, item_set),
                                  polarity = "bipolar",
                                  url = stimulus_url,
                                  instruction = shiny::p(
                                    # shiny::h4(
                                    #   psychTestR::i18n("TGAR_AAT_ITEM_HEADER",
                                    #                    sub = list(item_id = item_id,
                                    #                               num_stimuli = num_stimuli))),
                                    psychTestR::i18n(preamble_key)),
                                  anchors = FALSE,
                                  header = "double",
                                  header_style = header_style,
                                  sublabel_type = sublabel_type,
                                  reduce_labels = FALSE,
                                  style = style,
                                  trigger_button_text = psychTestR::i18n("CONTINUE"),
                                  failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                                  items = get_sub_group_items(sub_group, item_set, bipolar_items_sets[item_set]),
                                  choices = 0:(scale_length - 1),
                                  labels = get_sub_group_labels(sub_group, item_set, scale_length = scale_length),
                                  random_order = random_order,
                                  show_controls = TRUE,
                                  allow_download = FALSE,
                                  allow_na = allow_na[["bipolar"]],
                                  ...)))
  unipolar <- psychTestR::join(
    lapply(names(unipolar_items_sets), function(item_set){
      audio_radiobutton_matrix_page(label = sprintf("%s_%s", page_label, item_set),
                                    polarity = "unipolar",
                                    url = stimulus_url,
                                    instruction = shiny::p(
                                      # shiny::h4(
                                        # psychTestR::i18n("TGAR_AAT_ITEM_HEADER",
                                        #                  sub = list(item_id = item_id,
                                        #                             num_stimuli = num_stimuli))),
                                      psychTestR::i18n(preamble_key)),
                                    anchors = FALSE,
                                    header = "double",
                                    header_style = header_style,
                                    reduce_labels = FALSE,
                                    sublabel_type = "directed",
                                    style = style,
                                    trigger_button_text = psychTestR::i18n("CONTINUE"),
                                    failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                                    items = get_sub_group_items(sub_group, item_set, unipolar_items_sets[item_set]),
                                    choices = 0:(scale_length - 1),
                                    labels = get_sub_group_labels(sub_group, item_set, scale_length = scale_length),
                                    random_order = random_order,
                                    show_controls = TRUE,
                                    allow_download = FALSE,
                                    allow_na = allow_na[["unipolar"]],
                                    ...)
      }))
  #browser()
  psychTestR::join(bipolar, unipolar)

}
test_AAT <- function(subgroup = "a"){
  elts <- psychTestR::join(AAT(randomize_stimuli = T,
                               label = "AAT",
                               sub_group = subgroup,
                               allow_na = c(2, 0)),
                           psychTestR::new_timeline(
                             psychTestR::final_page(psychTestR::i18n("CLOSE_BROWSER")), dict = GAR::GAR_dict))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = c("en" = "Title", "de_f" = "Titel"),
                                                       admin_password = "admin_password",
                                                       researcher_email = "researcher_email",
                                                       languages = c("de_f")))

}
