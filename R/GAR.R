#' General Audio Rating modules
#'
#' This function defines a module for rating music or audio excerpts on a list of Likert items;
#' for incorporation into a
#' psychTestR timeline. Use this function if you want to include GAR in a
#' battery of other tests, or if you want to add custom psychTestR pages to
#' your test timeline. For a standalone implementation of the GAR, consider
#' using \code{GAR_standalone()}.
#'
#' @inheritParams audio_radiobutton_matrix_page
#'
#'
#' @export
GAR <- function(label = "EMO1",
                questionnaire = "EMO1",
                response_scale = "L7",
                items_prefix_pattern = "s%02d",
                item_order = NULL,
                num_stimuli = 10,
                num_rating_items = 6,
                anchors = FALSE,
                header = "double",
                header_style = NULL,
                reduce_labels = TRUE,
                style = default_style,
                allow_na = TRUE,
                audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                audio_type = "wav",
                dict = GAR::GAR_dict,
                random_order = FALSE,
                randomize_stimuli = FALSE,
                show_controls = TRUE,
                allow_download = FALSE,
                question_header_offset = 0,
                question_header_max = num_stimuli,
                vertical = FALSE,
                wait = TRUE,
                loop = FALSE,
                reverse_anchors = FALSE,
                ...) {
  #browser()
  dots <- list(...)
  MAS_IE <- FALSE
  if("MAS_IE" %in% names(dots)){
    MAS_IE <- dots$MAS_IE
  }
  quest <- get_questionnaires()
  if(!(questionnaire %in% quest$id)){
    stop(sprintf("Unknown questionnaire: %s", questionnaire))
  }
  if(questionnaire == "AAT"){
    return(AAT(label,
               num_stimuli = num_stimuli,
               audio_url = audio_url,
               audio_type = audio_type,
               random_order = random_order,
               allow_na = allow_na,
               sub_group = list(...)$sub_group))
  }
  num_rating_items <- max(1, min(num_rating_items, quest[quest$id == questionnaire,]$max_items))
  scale_length <- as.numeric(stringr::str_extract(response_scale, "[0-9]+"))

  preamble_key <- sprintf("TGAR_%s_PREAMBLE", questionnaire)
  prompt_key <- sprintf("TGAR_%s_%%04d_PROMPT", questionnaire)
  label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)
  #browser()
  if(!is.null(item_order)){
    stopifnot(length(item_order) == num_stimuli)
  }
  gar_pages <-
    lapply(1:num_stimuli, function(id){
      page_label <- sprintf("%s_%02d", label, id)
      if(!is.null(item_order)){
        stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                     item_order[id],
                                                     audio_type))
      }
      else{
        stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                     sprintf(items_prefix_pattern, id),
                                                     audio_type))
      }
      #browser()
      if(!vertical){
        psychTestR::join(
          psychTestR::new_timeline(
            audio_radiobutton_matrix_page(label = page_label,
                                          url = stimulus_url,
                                          instruction =  psychTestR::i18n(preamble_key),
                                          anchors = anchors,
                                          header = header,
                                          header_style = header_style,
                                          sublabel_type = "directed",
                                          reduce_labels = reduce_labels,
                                          reverse_anchors = reverse_anchors,
                                          style = style,
                                          trigger_button_text = psychTestR::i18n("CONTINUE"),
                                          failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                                          items = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                                          choices = 0:(scale_length-1),
                                          labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                                          random_order = random_order,
                                          show_controls = show_controls,
                                          allow_download = allow_download,
                                          allow_na = allow_na,
                                          wait = wait,
                                          loop = loop),
            dict = dict),
          psychTestR::elt_save_results_to_disk(complete = F))
      } else{
        psychTestR::join(
          psychTestR::new_timeline(
            audio_radiobutton_multi_NAFC_page(
              label = page_label,
              prompt =  psychTestR::i18n(preamble_key),
              items = sapply(1:num_rating_items, function(x)
                psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
              choices = as.character(0:(scale_length - 1)),
              labels = sapply(1:scale_length, function(x)
                psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
              audio_url = stimulus_url,
              audio_type = tools::file_ext(stimulus_url),
              #style = style,
              trigger_button_text = psychTestR::i18n("CONTINUE"),
              failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
              random_order = random_order,
              show_controls = show_controls,
              allow_download = allow_download,
              allow_na = allow_na,
              wait = wait,
              loop = loop
            ),
            dict = dict),
          psychTestR::elt_save_results_to_disk(complete = F))

      }

    })
  save_stimuli <- function(label){
    function(order, state, ...){
      #browser()
      if(!is.null(item_order)){
        stimuli <- sprintf("%s", item_order[1:num_stimuli])[order]
      }
      else{
        stimuli <- sprintf(items_prefix_pattern, 1:num_stimuli)[order]
      }#
      message(sprintf("Saving stimulus order for  %s (length: %d): %s",
                      label, length(order),
                      paste(stimuli, collapse = ", ")))
      psychTestR::save_result(state, label, stimuli)
    }}
  if(randomize_stimuli){
    psychTestR::join(
      psychTestR::begin_module(label = label),
      psychTestR::randomise_at_run_time(label,
                                        logic = gar_pages,
                                        save_order = save_stimuli(sprintf("%s_stimulus_order", label))),
      if(MAS_IE) MAS_item_evaluation(dict = dict),
      psychTestR::end_module()
    )
  }
  else{
    psychTestR::join(
      psychTestR::begin_module(label = label),
      psychTestR::order_at_run_time(label,
                                    logic = gar_pages,
                                    get_order = function(...) 1:num_stimuli,
                                    save_order = save_stimuli(sprintf("%s_stimulus_order", label))),
      if(MAS_IE) MAS_item_evaluation(dict = dict),
      # scoring
      psychTestR::end_module()
    )

  }
}

#' MAS_item_evaluation
#'
#' This function defines a module for assessing MAS items
#'
#'
#' @export
MAS_item_evaluation <- function(label = "MAS_IE",
                                style = default_style,
                                header_style = NULL,
                                random_order = FALSE,
                                vertical = TRUE,
                                dict = GAR::GAR_dict){
  psychTestR::join(
    psychTestR::begin_module("MAS_IE"),
    psychTestR::new_timeline(
      if(vertical){
        audio_radiobutton_multi_NAFC_page(
          label = label,
          prompt =  psychTestR::i18n("TGAR_MAS_IE_PREAMBLE"),
          items = sapply(1:18, function(x) psychTestR::i18n(sprintf("TGAR_MAS_%04d_PROMPT", x)), simplify = T, USE.NAMES = T),
          choices = as.character(0:4),
          labels = sapply(1:5, function(x) psychTestR::i18n(sprintf("TGAR_MAS_IE5_CHOICE%d", x)), simplify = T, USE.NAMES = T),
          audio_url = NULL,
          audio_type = NULL,
          trigger_button_text = psychTestR::i18n("CONTINUE"),
          failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
          random_order = random_order,
          allow_na = FALSE,
        )
        # ,
        # radiobutton_matrix_page(label = label,
        #                         instruction = psychTestR::i18n("TGAR_MAS_IE_PREAMBLE"),
        #                         anchors = FALSE,
        #                         header = "double",
        #                         header_style = header_style,
        #                         sublabel_type = c("directed"),
        #                         reduce_labels = FALSE,
        #                         style = style,
        #                         trigger_button_text = psychTestR::i18n("CONTINUE"),
        #                         failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
        #                         items = sapply(1:18, function(x) psychTestR::i18n(sprintf("TGAR_MAS_%04d_PROMPT", x)), simplify = T, USE.NAMES = T),
        #                         choices = 0:4,
        #                         labels = sapply(1:5, function(x) psychTestR::i18n(sprintf("TGAR_MAS_IE5_CHOICE%d", x)), simplify = T, USE.NAMES = T),
        #                         random_order = random_order,
        #                         allow_na = FALSE)

      }
      else{
        radiobutton_matrix_page(label = label,
                                instruction = psychTestR::i18n("TGAR_MAS_IE_PREAMBLE"),
                                anchors = FALSE,
                                header = "double",
                                header_style = header_style,
                                sublabel_type = c("directed"),
                                reduce_labels = FALSE,
                                style = style,
                                trigger_button_text = psychTestR::i18n("CONTINUE"),
                                failed_validation_message = psychTestR::i18n("ANSWER_MISSING"),
                                items = sapply(1:18, function(x) psychTestR::i18n(sprintf("TGAR_MAS_%04d_PROMPT", x)), simplify = T, USE.NAMES = T),
                                choices = 0:4,
                                labels = sapply(1:5, function(x) psychTestR::i18n(sprintf("TGAR_MAS_IE5_CHOICE%d", x)), simplify = T, USE.NAMES = T),
                                random_order = random_order,
                                allow_na = FALSE)
      },
      dict = dict
    ),
    psychTestR::end_module()
  )



}
