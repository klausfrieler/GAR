#' Standalone version of General Audio Rating questionnaires
#'
#' This function launches a standalone testing session for the General Audio scale
#'
#' @inheritParams GAR
#'
#'
#' @export
GAR_standalone <- function(label = "EMO1",
                           questionnaire = "EMO1",
                           response_scale = "L7",
                           items_prefix_pattern = "s%02d",
                           item_order = NULL,
                           num_stimuli = 10,
                           num_rating_items = 6,
                           anchors = FALSE,
                           header = "double",
                           reduce_labels = TRUE,
                           style = default_style,
                           allow_na = FALSE,
                           audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                           audio_type = "wav",
                           with_id = FALSE,
                           dict = GAR::GAR_dict,
                           random_order = FALSE,
                           randomize_stimuli = FALSE,
                           show_controls = TRUE,
                           allow_download = FALSE,
                           admin_password = "groove",
                           researcher_email = NULL,
                           languages = GAR::languages(),
                           validate_id = "auto",
                           ...) {
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
          psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                           placeholder = paste(psychTestR::i18n("EG"), "123456", sep = " "),
                           button_text = psychTestR::i18n("CONTINUE"),
                           validate = validate_id),
      dict = dict
    ),
    GAR(label = label,
        questionnaire = questionnaire,
        response_scale = response_scale,
        items_prefix_pattern = items_prefix_pattern,
        item_order = item_order,
        num_stimuli = num_stimuli,
        num_rating_items = num_rating_items,
        header = header,
        anchors = anchors,
        reduce_labels = reduce_labels,
        style = style,
        audio_url = audio_url,
        audio_type = audio_type,
        dict = dict,
        allow_na = allow_na,
        random_order = random_order,
        randomize_stimuli = randomize_stimuli,
        show_controls = show_controls,
        allow_download = allow_download,
        ...),
    psychTestR::elt_save_results_to_disk(complete = T),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(psychTestR::i18n("RESULTS_SAVED"),
                                      psychTestR::i18n("CLOSE_BROWSER"))),
      dict = dict)
    )
  title <- unlist(rlang::set_names(
    purrr::map(GAR::languages(), function(x)
      GAR::GAR_dict$translate(sprintf("TGAR_%s_TITLE", questionnaire), x)),
    GAR::languages()
  ))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = title,
                                                       admin_password = admin_password,
                                                       researcher_email = researcher_email,
                                                       languages = languages))
}
