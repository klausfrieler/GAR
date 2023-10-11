#' Audio Attention Study Modules
#'
#' This function defines module for aat_item_evaluation
#' @param label (character scale) Label for the whole shit
#' @param scale_length (integer scalar) Length of agreement scale to be used (5 or 7)
#' @param nested (boolean) Asked two question per item all questions per item?
#' @param max_items (integer) For debugging, only use max_items (max 43)
#' @param dict (psychTestR dictionary object) You really want another dictionary?
#' @export
aat_item_evaluation <- function(label = "AAT_ITEM_EVAL",
                                scale_length = 5,
                                nested = F,
                                dict = GAR::GAR_dict,
                                max_items = 43,
                                with_intro = T,
                                sep = " ... ",
                                ...) {
  items <- GAR_dict %>%
    as.data.frame() %>%
    filter(stringr::str_detect(key, "TGAR_AAT_M")) %>%
    pull(key)

  items <- items[1:max(min(max_items, 43), 1)]
  style_block <- "text-align:left;width:40%;margin-left:30%;margin-right:30%;margin-top:1em;margin-bottom:1em;"
  style_single <- "text-align:center;width:60%;margin-left:20%;margin-right:20%;margin-top:1em;margin-bottom:1em;"

  overall_intro_page <- psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::h4(
      psychTestR::i18n("TGAR_ITEM_EVAL_INTRO", sub = list(num_pairs = max_items)),
      style = style_block),
    button_text = psychTestR::i18n("CONTINUE")),
    dict = GAR::GAR_dict)

  opposite_intro_page <- psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::h4(
      psychTestR::i18n("TGAR_ITEM_EV1_INTRO"),
      style = style_block),
    button_text = psychTestR::i18n("CONTINUE")),
    dict = GAR::GAR_dict)

  suitability_intro_page <- psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::h4(
      psychTestR::i18n("TGAR_ITEM_EV2_INTRO"),
      style = style_block),
    button_text = psychTestR::i18n("CONTINUE")),
    dict = GAR::GAR_dict)

  if(nested){
    pages <- psychTestR::join(
      lapply(items, function(ik)
        psychTestR::new_timeline(
          make_aat_item_eval_pages(ik, scale_length =  scale_length, pages = 1:2),
          dict = dict)))
  }
  else{
    pages1 <- lapply(items, function(ik)
      psychTestR::new_timeline(
        make_aat_item_eval_pages(ik, scale_length =  scale_length, pages = 1),
        dict = dict))
    pages2 <- lapply(items, function(ik)
      psychTestR::new_timeline(
        make_aat_item_eval_pages(ik, scale_length =  scale_length, pages = 2),
        dict = dict))
    pages <- psychTestR::join(if(with_intro) opposite_intro_page,
                              pages1,
                              if(with_intro) suitability_intro_page,
                              pages2)

  }
  psychTestR::join(
    psychTestR::begin_module(label = label),
    if(with_intro) overall_intro_page,
    pages,
    psychTestR::end_module()
  )

}

make_aat_item_eval_pages <- function(item_key, dict = GAR::GAR_dict, scale_length = 5, pages = 1:2, sep = " ... "){
  #browser()
  page_label <- sprintf(stringr::str_remove(item_key, "TGAR_AAT_") %>% str_remove("_PROMPT"))
  label_keys <- sprintf("TGAR_AAT_L%s_CHOICE%%01d", scale_length)
  choices <- as.character(1:scale_length)
  button_style <- sprintf("min-width:%dpx", ifelse(scale_length == 7, 250, 0))
  page1 <-  psychTestR::NAFC_page(label = sprintf("%s_1", page_label),
                                  prompt = shiny::p(
                                    shiny::h3(shiny::tags$b(
                                      stringr::str_replace(psychTestR::i18n(item_key), "_", sep))),
                                  shiny::h4(psychTestR::i18n("TGAR_AAT_EV1_PREAMBLE_SINGLE"),
                                            style = "margin-top:1em;margin-bottom:1em;")),
                                  choices = choices,
                                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_keys, x)), simplify = T, USE.NAMES = T),
                                  save_answer = T,
                                  button_style = button_style,
                                  arrange_vertically = scale_length > 5)
  page2 <-  psychTestR::NAFC_page(label = sprintf("%s_2", page_label),
                                  prompt = shiny::p(
                                    shiny::h3(shiny::tags$b(stringr::str_replace(psychTestR::i18n(item_key), "_", sep))),
                                  shiny::h4(psychTestR::i18n("TGAR_AAT_EV2_PREAMBLE_SINGLE"),
                                            style = "margin-top:1em;margin-bottom:1em;")),
                                  choices = choices,
                                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_keys, x)), simplify = T, USE.NAMES = T),
                                  save_answer = T,
                                  arrange_vertically = scale_length > 5,
                                  button_style = button_style)
  page_list <- c(page1, page2)
  psychTestR::join(page_list[pages])
}

test_aat_item_eval_pages <- function(scale_length = 5, nested = F){
  aat_items <- aat_item_evaluation(nested = nested, scale_length = scale_length)
  elts <- psychTestR::join(aat_items,
                           psychTestR::code_block(function(state, ...){
                             res <- psychTestR::get_results(state, complete = T)
                             #browser()
                           }),
                           psychTestR::new_timeline(
                             psychTestR::final_page(psychTestR::i18n("CLOSE_BROWSER")), dict = GAR::GAR_dict))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = c("en" = "Title", "de" = "Titel"),
                                                       admin_password = "admin_password",
                                                       researcher_email = "researcher_email",
                                                       languages = c("de")))
}
