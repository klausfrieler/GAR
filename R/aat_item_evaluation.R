#' Audio Attention Study Modules
#'
#' This function defines module for aat_item_evaluation
#' @param label (character scale) Label for the whole shit
#' @param scale_length (integer scalar) Length of agreement scale to be used (5 or 7)
#' @param nested (boolean) Asked two question per item all questions per item?
#' @param dict (psychTestR dictionary object) You really want another dictionary?
#' @export
aat_item_evaluation <- function(label = "AAT_EVAL",
                                scale_length = 5,
                                nested = F,
                                dict = GAR::GAR_dict,
                ...) {
  items <- GAR_dict %>%
    as.data.frame() %>%
    filter(stringr::str_detect(key, "TGAR_AAT_M")) %>%
    pull(key)
  items <- items[1:5]
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
    pages <- psychTestR::join(pages1, pages2)

  }
  psychTestR::join(
    psychTestR::begin_module(label = label),
    pages,
    psychTestR::end_module()
  )

}

make_aat_item_eval_pages <- function(item_key, dict = GAR::GAR_dict, scale_length = 5, pages = 1:2){
  #browser()
  sep <- "..."
  page_label <- sprintf(str_remove(item_key, "TGAR_AAT_") %>% str_remove("_PROMPT"))
  label_keys <- sprintf("TGAR_AAT_L%s_CHOICE%%01d", scale_length)
  choices <- as.character(1:scale_length)
  button_style <- sprintf("min-width:%dpx", ifelse(scale_length == 7, 250, 0))
  page1 <-  psychTestR::NAFC_page(label = sprintf("%s_1", page_label),
                                  prompt = shiny::p(
                                    shiny::h4(psychTestR::i18n("TGAR_AAT_EV1_PREAMBLE_SINGLE")),
                                    shiny::h4(shiny::tags$b(
                                      stringr::str_replace(psychTestR::i18n(item_key), "_", sep)))),
                                  choices = choices,
                                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_keys, x)), simplify = T, USE.NAMES = T),
                                  save_answer = T,
                                  button_style = button_style,
                                  arrange_vertically = scale_length > 5)
  page2 <-  psychTestR::NAFC_page(label = sprintf("%s_2", page_label),
                                  prompt = shiny::p(
                                    shiny::h4(psychTestR::i18n("TGAR_AAT_EV2_PREAMBLE_SINGLE")),
                                    shiny::h4(shiny::tags$b(
                                      stringr::str_replace(psychTestR::i18n(item_key), "_", sep)))),
                                  choices = choices,
                                  labels = sapply(1:scale_length, function(x) psychTestR::i18n(sprintf(label_keys, x)), simplify = T, USE.NAMES = T),
                                  save_answer = T,
                                  arrange_vertically = scale_length > 5,
                                  button_style = button_style)
  page_list <- c(page1, page2)
  psychTestR::join(page_list[pages])
}

test_aat_item_eval_pages <- function(scale_length = 5){
  aat_items <- aat_item_evaluation(nested = T, scale_length = scale_length)
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
