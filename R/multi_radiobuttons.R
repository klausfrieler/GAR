#' New radiobutton NAFC page
#'
#' Creates a radiobutton n-alternative forced choice page.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?") or an object of
#' class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' radiobutton labels.
#' If named, then values will be used for radiobutton IDs and names will be used
#' for radiobutton labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the radiobutton NAFC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param failed_validation_message (Character scalar) Text to be displayed
#' when validation fails.
#'
#' @param save_answer (Boolean scalar) Whether or not to save the answer.
#'
#' @param hide_response_ui (Boolean scalar) Whether to begin with the response
#' interface hidden (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id (Character scalar) HTML ID for the response user interface.
#'
#'
#' @export
audio_radiobutton_multi_NAFC_page <-
  function(label,
           prompt,
           items = "",
           choices,
           labels = NULL,
           trigger_button_text = "Continue",
           failed_validation_message = "Answer missing!",
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           random_order = T,
           on_complete = NULL,
           audio_url = "https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/GAR/EMO1",
           audio_type = "wav",
           btn_play_prompt = if (!show_controls) "Click here to play",
           show_controls = FALSE,
           allow_download = FALSE,
           allow_na = FALSE,
           wait = TRUE,
           loop = FALSE,
           admin_ui = NULL) {
    stopifnot(
      is.scalar.character(label),
      is.scalar.character(trigger_button_text),
      is.scalar.character(failed_validation_message),
      is.character(choices),
      length(choices) > 0L
    )
    audio_ui <- NULL
    if(!is.null(audio_url)){
      audio_ui <- shiny::tags$div(shiny::tags$audio(
        shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
        shiny::tags$source(src = audio_url, type = paste0("audio/", audio_type)),
        id = "media",
        preload = "auto",
        autoplay = "autoplay",
        loop = if (loop) "loop",
        oncanplaythrough = media.js$show_media_btn,
        onplay = paste0(media.js$media_played, media.js$hide_media_btn),
        onended = if (wait) media.js$show_responses else "null",
        controls = if (show_controls) "controls",
        controlsList = if (!allow_download) "nodownload"
      ), media_mobile_play_button(btn_play_prompt))

    }

    prompt2 <- shiny::tags$div(tagify(prompt),
                                    #shiny::span(url, style = "color:red"),
                                    audio_ui)
    item_order <- 1:length(items)
    if(random_order){
      item_order <- sample(1:length(items))
      items <- items[item_order]
    }

    ui <- shiny::div(
      tagify(prompt2),
      make_ui_radiobutton_multi_NAFC(
        label,
        choices,
        items = items,
        labels = labels,
        trigger_button_text = trigger_button_text,
        hide = hide_response_ui,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...){
      rv <- shiny:: reactiveValuesToList(input)
      rv[sprintf("%s_%d", label, 1:length(items))]
    }
    validate <- function(answer, ...){
      if (sum(sapply(answer, is.null)) == 0) {
        TRUE
      } else {
        failed_validation_message
      }

    }
    psychTestR::page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make NAFC radiobuttons
#'
#' Creates HTML code for n-alternative forced-choice response radiobutton options.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param subprompt (Character scalar) Optional additional text in bold letters
#' below the prompt.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param hide (Boolean scalar) Whether the radiobuttons should be hidden (possibly to be shown later).
#'
#' @param id (Character scalar) HTML ID for the div containing the radiobuttons.
#'
#' @export
make_ui_radiobutton_multi_NAFC <-
  function(label,
           items,
           choices,
           labels = NULL,
           trigger_button_text = "Continue",
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.character(choices) && length(choices) > 0L,
      is.character(items),
      is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choices)
        )
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choices)))
        choices
      else
        names(choices)
    }
    labels <-
      purrr::map(labels, function(label)
        shiny::tags$span(style = "font-size: 15px; line-height: 15px; min-height: 15px", label))


    radiobuttons_div <-
      purrr::map(1:length(items), function(idx){
        item_div <- shiny::tags$div(items[idx], style = "text-align: left; max-width:390px; color: #1f77b4;font-weight: bold")
        shiny::tags$div(
          style = "text-align: left;",
          item_div,
          shiny::tags$div(
            style = "display: table; margin: 0 auto;",
            shiny::tags$div(
              style = "display: inline-block; width: 100%;",
              shiny::radioButtons(
                inputId = sprintf("%s_%d", label, idx),
                label = "",
                choiceNames = labels,
                choiceValues = choices,
                selected = ""
              )
            )
          )
        )
      }) %>% shiny::tagList()

    shiny::tags$div(
      id = id,
      style = "display: inline-block",
      radiobuttons_div,
      psychTestR::trigger_button("next", trigger_button_text)
    )
  }
