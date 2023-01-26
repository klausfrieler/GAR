#' New multiple radiobutton NAFC page
#'
#' Creates a multiple radiobutton n-alternative forced choice page.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompts (Character vector) Prompts to be displayed over the response
#' choices
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
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
#' See \link[psychTestR]{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id (Character scalar) HTML ID for the response user interface.
#'
#' @inheritParams make_ui_multi_radiobutton_NAFC
#' @inheritParams psychTestR::page
#'
#' @export
multi_radiobutton_NAFC_page <- function(label,
                                        prompts,
                                        choices,
                                        instruction = NULL,
                                        labels = NULL,
                                        trigger_button_text = "Continue",
                                        failed_validation_message = "Answer missing!",
                                        save_answer = TRUE,
                                        hide_response_ui = FALSE,
                                        random_order = FALSE,
                                        response_ui_id = "response_ui",
                                        on_complete = NULL,
                                        admin_ui = NULL) {
  stopifnot(
    is.scalar.character(label),
    is.character.vector(prompts),
    is.scalar.character(trigger_button_text),
    is.scalar.character(failed_validation_message),
    is.character.or.numeric(choices),
    length(choices) > 0L
  )

  instruction_tag <- NULL

  if(!is.null(instruction)) {
    instruction_tag <- tagify(instruction)
  }
  ui <- shiny::tags$div(instruction_tag,
                        make_ui_multi_radiobuttons(label,
                                                   items = prompts,
                                                   scale_labels = labels,
                                                   choices,
                                                   trigger_button_text = trigger_button_text,
                                                   hide = hide_response_ui,
                                                   id = response_ui_id))
  itemlist <- 1:length(prompts)
  names(itemlist) <- sprintf("item_%02d", 1:length(prompts))
  get_answer <- function(input, ...) {
    browser()
    radios <- reactiveValuesToList(input)
    values <- radios[str_detect(names(radios), "^item_[0-9]+")] %>%
      unlist() %>%
      names() %>%
      str_extract_all("[0-9]+")
    answer <- purrr::map(values, function(x){
      as.numeric(x[[1]])
    })
    names(answer) <- sprintf("%s.q%d", label, 1:length(values))
    print(answer)
    answer

  }

  validate <- function(answer, ...) {
    browser()
    if (sum(unlist(lapply(1:length(prompts), function(i) !is.null(answer[[i]])))) == length(prompts)) {
      TRUE
    } else {
      failed_validation_message
    }
  }
  psychTestR::page(ui = ui,
                   label = label,
                   get_answer = get_answer,
                   save_answer = save_answer,
                   validate = validate,
                   on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui
  )
}

#' Make multiple NAFC radiobuttons with the same choices
#'
#' Creates the HTML code for multiple n-alternative forced choice response
#' radiobutton options
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompts (Character vector) Prompts to be displayed over the response
#' choices
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param hide (Boolean scalar) Whether the radiobuttons should be hidden
#' (possibly to be shown later).
#'
#' @param random_order (Boolean scalar) Whether the order of the items should
#' be randomized.
#'
#' @param id (Character scalar) HTML ID for the div containing the radiobuttons.
#'
#' @export
make_ui_multi_radiobutton_NAFC <- function(label,
                                           prompts,
                                           choices,
                                           labels = NULL,
                                           trigger_button_text = "Continue",
                                           hide = FALSE,
                                           random_order = FALSE,
                                           id = "response_ui") {
  stopifnot(
    is.character.or.numeric(choices) && length(choices) > 0L,
    is.scalar.logical(hide),
    is.scalar.logical(random_order),
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
      shiny::tags$span(style = "font-size: 15px; line-height: 15px;", label))

  order <- if(random_order) {sample(1:length(prompts))} else 1:length(prompts)
  #browser()
  reduced_labels <- rep("", length(labels))
  reduced_labels[1] <- labels[1]
  reduced_labels[length(reduced_labels)] <- labels[length(reduced_labels)]

  multi_radiobuttons_div <- shiny::tags$div(
    mapply(function(prompt_number) {
      shiny::tags$div(style = "margin-left:10%;width:80%;text-align: left;",
                      shiny::tags$b(prompts[prompt_number]),
                      shiny::radioButtons(inputId = paste0("item", prompt_number),
                                          label = "",
                                          choiceNames = reduced_labels,
                                          choiceValues = choices,
                                          inline = TRUE,
                                          selected = character(0)))
    }, order, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )

  shiny::tags$div(id = id,
                  style = "inline-block;",
                  multi_radiobuttons_div,
                  psychTestR::trigger_button("next", trigger_button_text)
  )
}



get_radio_button_row <- function(prompt, group_id, values, label = ""){
  if(is.scalar.character(prompt))
    prompt <- tagify(prompt)
  style <- "border:1px solid black;max-width:250px;min-width:250px;padding:0px;text-align:center;vertical-align:center"
  row <-
    tdRadioButtons(inputId = group_id,
                   label = label,
                   choiceNames = rep("", length(values)),
                   choiceValues = values,
                   inline = TRUE,
                   selected = NULL)

  shiny::tags$tr(
    shiny::tags$td(prompt,
                   style = "font-size:12pt;vertical-align:center;border:1px solid black;text-align:left;display:block;padding:2px"),
    shiny::tagList(row))
}


make_ui_multi_radiobuttons <- function(label,
                                       items = NULL,
                                       scale_labels = NULL,
                                       choices = 1:length(scale_labels),
                                       reduce_labels = TRUE,
                                       trigger_button_text = "Continue",
                                       hide = FALSE,
                                       id = "response_ui") {
  stopifnot(
    is.character.or.numeric(choices) && length(choices) > 0L,
    is.scalar.logical(hide)
  )
  if (is.null(scale_labels)) {
    scale_labels <- if (is.null(names(choices)))
      choices
    else
      names(choices)
  }
  reduced_labels <- rep("", length(scale_labels))
  reduced_labels[1] <- scale_labels[1]
  reduced_labels[length(reduced_labels)] <- scale_labels[length(reduced_labels)]
  if(reduce_labels){
    scale_labels <- reduced_labels
  }
  header_row1 <-
    shiny::tags$tr(
      shiny::tags$td(""),
      shiny::tagList(purrr::map(scale_labels, function(label)
      shiny::tags$td(style = "font-size: 15px; line-height: 15px;border:1px solid black;width:15px", label))))

  #row1_style <- "font-size: 15px; line-height: 15px;border:1px solid black;float:left; display:block"
  row1_style  <- ""
  row2_style  <- "font-size: 12pt;line-height:24pt;border:1px solid black;text-align:center"
  table_style <- "border:1px solid black;line-height:24pt;margin-bottom:12pt"
  #n <- length(scale_labels)
  # eff_labels <-scale_labels
  # eff_labels[2:(n-1)] <- ""

  header_row1 <-
    shiny::tags$tr(
      purrr::map(c("", scale_labels), ~{shiny::tags$td(.x, style = row1_style)})
  )

  header_row2 <-
    shiny::tags$tr(
      shiny::tags$td(""),
      shiny::tagList(purrr::map(1:length(scale_labels), function(label)
        shiny::tags$td(style = row2_style, label))))

  #browser()
  table_body <- shiny::tags$div(
    purrr::map(1:length(items),
          function(idx) {#
            get_radio_button_row(prompt = shiny::span(items[idx]),
                                 group_id = sprintf("item_%02d", idx),
                                 values = 1:length(scale_labels))
    })
  )
  item_table <-
    shiny::tags$table(
      header_row1,
      header_row2,
      table_body,
      style = table_style
    )

  shiny::tags$div(id = id,
                  style = "inline-block;",
                  item_table,
                  psychTestR::trigger_button("next", trigger_button_text)
  )
}

media.js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

media_mobile_play_button <- function(btn_play_prompt) shiny::tags$p(
  shiny::tags$strong(btn_play_prompt,
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media.js$play_media))
#' Make multiple radiobutton NAFC audio page
#'
#' Creates a multiple radiobutton n-alternative forced choice page for a single audio file.
#'
#' @param url URL to the audio. Can be an absolute URL (e.g.
#' "http://mysite.com/audio.mp3") or a URL relative to the /www directory (e.g.
#' "audio.mp3").
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param show_controls Whether or not to show audio controls to the
#' participant, so that they can control audio playback.
#' @param allow_download Whether the participant is given a button to download
#' the audio file; only relevant if \code{show_controls} is \code{TRUE}.
#' @param wait Whether to wait for the audio to finish before displaying the
#' response buttons.
#' @param loop Whether the audio should loop.
#'
#' @inherit multi_radiobutton_NAFC_page
#' @inherit psychTestR::page
#' @inherit make_ui_multi_radiobutton_NAFC
#'
#' @export
audio_multi_radiobutton_NAFC_page <- function(label,
                                              prompts,
                                              choices,
                                              url,
                                              instruction = "",
                                              labels = NULL,
                                              type = tools::file_ext(url),
                                              trigger_button_text = "Continue",
                                              failed_validation_message = "Answer missing!",
                                              save_answer = TRUE,
                                              hide_response_ui = FALSE,
                                              random_order = FALSE,
                                              response_ui_id = "response_ui",
                                              on_complete = NULL,
                                              wait = TRUE,
                                              loop = FALSE,
                                              admin_ui = NULL,
                                              btn_play_prompt = if (!show_controls) "Click here to play",
                                              show_controls = FALSE,
                                              allow_download = FALSE) {
  stopifnot(is.scalar.character(label),
            is.character.or.numeric(choices),
            is.scalar.character(url),
            is.scalar.logical(wait),
            is.scalar.logical(loop),
            is.scalar.logical(show_controls),
            is.scalar.logical(allow_download),
            is.scalar.logical(hide_response_ui))
  audio_ui <- shiny::tags$div(shiny::tags$audio(
    shiny::tags$head(shiny::tags$script(shiny::HTML(media.js$media_not_played))),
    shiny::tags$source(src = url, type = paste0("audio/", type)),
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
  #browser()
  deselect_script <- sprintf("
  for(var i = 1; i <= %s; ++i ){
    var radios = document.getElementsByName('item_0' + i);
    for(var j = 0; j < radios.length; ++j){
      radios[j].checked = false;
    }
  }", length(choices))
  deselect_script_tag <- (shiny::tags$script(shiny::HTML(deselect_script)))
  instruction2 <- shiny::tags$div(tagify(instruction),
                                  audio_ui, deselect_script_tag)

  multi_radiobutton_NAFC_page(label = label, prompts = prompts, choices = choices, labels = labels,
                              trigger_button_text = trigger_button_text,
                              failed_validation_message = failed_validation_message,
                              save_answer = save_answer,
                              hide_response_ui = hide_response_ui,
                              random_order = random_order,
                              response_ui_id = response_ui_id,
                              on_complete = on_complete,
                              admin_ui = admin_ui,
                              instruction = instruction2)
}
