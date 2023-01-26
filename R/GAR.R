#' General Audio Ration component
#'
#' This function defines a module for rating music or audio excerpts on a list of Likert items;
#' for incorporation into a
#' psychTestR timeline. Use this function if you want to include GAR in a
#' battery of other tests, or if you want to add custom psychTestR pages to
#' your test timeline. For a standalone implementation of the GAR, consider
#' using \code{GAR_standalone()}.
#'
#' @inheritParams audio_multi_radiobutton_NAFC_page
#'
#'
#' @export
GAR <- function(label = "EMO1",
                type = "EMO1",
                response_scale = "L7",
                items_prefix_pattern = "s%02d",
                num_stimuli = 10,
                num_rating_items = 6,
                audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
                audio_type = "wav",
                dict = GAR::GAR_dict,
                random_order = FALSE,
                show_controls = TRUE,
                allow_download = FALSE,
                ...) {
  browser()
  scale_length <- as.numeric(stringr::str_extract(response_scale, "[0-9]+"))

  preamble_key <- sprintf("TGAR_%s_PREAMBLE", type)
  prompt_key <- sprintf("TGAR_%s_%%04d_PROMPT", type, num_rating_items)
  label_key <- sprintf("TGAR_%s_CHOICE%%01d", response_scale)

  gar <- psychTestR::join(
    lapply(1:num_stimuli, function(id){
      page_label <- sprintf("%s_%02d", label, id)
      stimulus_url <- file.path(audio_url, sprintf("%s.%s",
                                                   sprintf(items_prefix_pattern, id),
                                                   audio_type))
      #browser()
      psychTestR::new_timeline(
        audio_multi_radiobutton_NAFC_page(label = page_label,
                                          url = stimulus_url,
                                          instruction = psychTestR::i18n(preamble_key),
                                          prompts = sapply(1:num_rating_items, function(x) psychTestR::i18n(sprintf(prompt_key, x)), simplify = T, USE.NAMES = T),
                                          choices = 0:(scale_length-1),
                                          labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_key, x)), simplify = T, USE.NAMES = T),
                                          random_order = random_order,
                                          show_controls = show_controls,
                                          allow_download = allow_download,
                                          ...),
        dict = dict)
    }))
  browser()

  psychTestR::join(
    psychTestR::begin_module(label = label),
                   gar,
                   # scoring
                   psychTestR::end_module()
                   )
}
