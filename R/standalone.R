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
                           num_stimuli = 10,
                           num_rating_items = 6,
                           anchors = FALSE,
                           header = "double",
                           reduce_labels = TRUE,
                           style = list(label_widths = list("100px", "100px"),
                                        div_style = "width:60%;margin-left:20%;margin-right:20%"),
                           allow_na = FALSE,
                           audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                           audio_type = "wav",
                           with_id = FALSE,
                           dict = GAR::GAR_dict,
                           random_order = FALSE,
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
        show_controls = show_controls,
        allow_download = allow_download,
        ...),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(psychTestR::i18n("RESULTS_SAVED"),
                                      psychTestR::i18n("CLOSE_BROWSER"))),
      dict = dict)
    )
  title <- unlist(setNames(
    purrr::map(GAR::languages(), function(x)
      GAR_dict$translate(sprintf("TGAR_%s_TITLE", questionnaire), x)),
    GAR::languages()
  ))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = title,
                                                       admin_password = admin_password,
                                                       researcher_email = researcher_email,
                                                       languages = languages))
}
