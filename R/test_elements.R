default_style <-
  list(
    unipolar = list(row_id_style = "min-width:200px;white-space:normal;",
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
#' New radio button matrix page
#'
#' Creates a  radio button matrix pag for n items with m Likert-type choices
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompts (Character vector) Prompts to be displayed over the response
#' choices
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radio button IDs and for
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
#' @inheritParams make_ui_radiobutton_matrix
#' @inheritParams psychTestR::page
#'
#' @export
radiobutton_matrix_page <- function(label,
                                    polarity = c("unipolar", "bipolar"),
                                    items,
                                    choices,
                                    instruction = NULL,
                                    labels = NULL,
                                    anchors = FALSE,
                                    header = c("double", "simple_str", "simple_num"),
                                    reduce_labels = TRUE,
                                    style = default_style,
                                    trigger_button_text = "Continue",
                                    allow_na = FALSE,
                                    failed_validation_message = "Answer missing!",
                                    save_answer = TRUE,
                                    hide_response_ui = FALSE,
                                    random_order = FALSE,
                                    response_ui_id = "response_ui",
                                    on_complete = NULL,
                                    admin_ui = NULL) {
  stopifnot(
    is.scalar.character(label),
    is.character.vector(items),
    is.scalar.character(trigger_button_text),
    is.scalar.character(failed_validation_message),
    is.character.or.numeric(choices),
    length(choices) > 0L
  )
  instruction_tag <- NULL

  if(!is.null(instruction)) {
    instruction_tag <- tagify(instruction)
  }
  item_order <- 1:length(items)
  if(random_order){
    item_order <- sample(1:length(items))
    items <- items[item_order]
  }
  ui <- shiny::tags$div(instruction_tag,
                        make_ui_radiobutton_matrix(label,
                                                   polarity = polarity,
                                                   items = items,
                                                   scale_labels = labels,
                                                   choices,
                                                   style = style,
                                                   trigger_button_text = trigger_button_text,
                                                   anchors = anchors,
                                                   header = header,
                                                   reduce_labels = reduce_labels,
                                                   hide = hide_response_ui,
                                                   id = response_ui_id))
  itemlist <- 1:length(items)
  names(itemlist) <- sprintf("item_%02d", 1:length(items))
  get_answer <- function(input, ...) {
    values <-shiny:: reactiveValuesToList(input)$radio_matrix
    answer <- purrr::map(values, function(x){
      if(!is.null(x[[1]])) as.numeric(x[[1]])
         else NA
    }) %>% unlist()
    #names(answer) <- stringr::str_remove_all(stringr::str_replace_all(tolower(names(answer)), " ", "_"), "[.]")

    names(answer) <- sprintf("%s.q%d", label, item_order)
    print(answer)
    answer

  }

  validate <- function(answer, allow_NA = allow_na, ...) {
    valid  <- TRUE
    if(!allow_NA){
      if(any(is.na(answer))){
        valid <- failed_validation_message
      }
    }
    valid
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

#' New radio button matrix page
#'
#' Creates a  radio button matrix pag for n items with m Likert-type choices
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param items (Character vector) Items to be displayed over the response
#' choices
#'
#' @param scale_labels Optional vector of labels for the radio button choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radio button IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param hide (Boolean scalar) Whether to begin with the response
#' interface hidden (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \link[psychTestR]{audio_NAFC_page} for an example.).
#'
#' @param label (character scalar) Label for the buttong matrix
#' @param polarity ("unipolar" or "bipolar") Determines whether scale will be unipolar (one label) or bipolar (two labels)
#' @param reduce_labels (boolean) Shall a only the endpoints of choice labels being labelled?
#' @param anchors (boolean)  Only for unipolar items: show scale labels?
#' @param style (character scalar) CSS string for the radiobuttons
#' @param header (character scale) One of simple_num, simple_str or double. simple_num only displays numbers in the header, simple_str uses verbal labels given in parameter choices, double used both.
#' @inheritParams psychTestR::page
#'
#' @export
make_ui_radiobutton_matrix <- function(label,
                                       polarity = c("unipolar", "bipolar"),
                                       items = NULL,
                                       scale_labels = NULL,
                                       choices = 1:length(scale_labels),
                                       reduce_labels = TRUE,
                                       anchors = TRUE,
                                       style = default_style,
                                       header = c("simple_num",
                                                  "simple_str",
                                                  "double"),
                                       trigger_button_text = "Continue",
                                       hide = FALSE,
                                       id = "response_ui") {
  stopifnot(
    is.character.or.numeric(choices) && length(choices) > 0L,
    is.scalar.logical(hide)
  )
  polarity <- match.arg(polarity)
  header <- match.arg(header)
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

  if(header == "simple_num"){
    if(polarity == "unipolar") {
      choiceNames <- 1:length(scale_labels)
    }
    else{
      choiceNames <- abs(choices)
    }
  }
  else if(header == "simple_str"){
    choiceNames <- scale_labels
  }
  else if(header == "double"){
    #browser()
    if(polarity == "unipolar") {
      sub_labels <- 1:length(scale_labels)
    }
    else{
      sub_labels <- abs(choices)
    }

    choiceNames <- lapply(1:length(scale_labels), function(i)
      shiny::tags$span(scale_labels[i], shiny::tags$br(), sub_labels[[i]]))

  }
  if(polarity == "unipolar"){
    if(anchors){
      rowLLabels <- rep(scale_labels[1], length(items))
      rowRLabels <- rep(scale_labels[length(scale_labels)], length(items))
    }
    else{
      rowLLabels <- rep("", length(items))
      rowRLabels <- rep("", length(items))
    }
  }
  else{
    ssplit <- stringr::str_split_fixed(items, "_", 2)
    rowLLabels <- ssplit[, 1]
    rowRLabels <- ssplit[, 2]
  }
  SRM_label_styles <- list(
    "unipolar" = list(
      "left" = "text-align:left",
      "right" = "text-align:left"),
    "bipolar" = list(
      "left" = sprintf("text-align:left;min-width:%s", style$bipolar_item_width_left),
      "right" = sprintf("text-align:left;min-width:%s", style$bipolar_item_width_right))
  )

  #browser()
  rowIDs <- lapply(items, function(it){
      shiny::div(it, style = style[[polarity]]$row_id_style)
  })
  # if(polarity == "unipolar"){
  #   browser()
  # }
  # print(length((rowIDs)))
  # print(length(unique(rowIDs)))
  #browser()
  item_table <- shinyRadioMatrix::radioMatrixInput(inputId = "radio_matrix",
                                                   rowLLabels = rowLLabels,
                                                   rowRLabels = rowRLabels,
                                                   rowIDs = rowIDs,
                                                   rowIDsName = "",
                                                   #choices = 1:length(reduced_labels),
                                                   choiceNames = choiceNames,
                                                   choiceValues = choices,
                                                   selected = NULL,
                                                   labelsWidth = style[[polarity]]$label_widths,
                                                   LLabelStyle = style[[polarity]]$label_styles[["left"]],
                                                   RLabelStyle = style[[polarity]]$label_styles[["right"]],
                                                   choiceStyle = "",
                                                   LLabPos = 0,
                                                   RLabPos = length(reduced_labels) + 1)
  shiny::tags$div(id = id,
                  shiny::tags$div(
                    style = style$div_style,
                    item_table),
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
#' Make radiobutton matrix audio page
#'
#' Creates a  radiobutton matrix page for a single audio file.
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
#' @inherit radiobutton_matrix_page
#' @inherit psychTestR::page
#' @inherit make_ui_radiobutton_matrix
#'
#' @export
audio_radiobutton_matrix_page <- function(label,
                                          polarity = c("unipolar", "bipolar"),
                                          items,
                                          choices,
                                          url,
                                          instruction = "",
                                          labels = NULL,
                                          anchors = FALSE,
                                          header = "double",
                                          style = default_style,
                                          reduce_labels = TRUE,
                                          trigger_button_text = "Continue",
                                          allow_na = FALSE,
                                          failed_validation_message = "Answer missing!",
                                          save_answer = TRUE,
                                          hide_response_ui = FALSE,
                                          random_order = FALSE,
                                          response_ui_id = "response_ui",
                                          on_complete = NULL,
                                          audio_type = tools::file_ext(url),
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
    shiny::tags$source(src = url, type = paste0("audio/", audio_type)),
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
  instruction2 <- shiny::tags$div(tagify(instruction),
                                  shiny::span(url, stylde = "color:red"),
                                  audio_ui)

  radiobutton_matrix_page(label = label,
                          polarity = polarity,
                          items = items,
                          choices = choices,
                          labels = labels,
                          anchors = anchors,
                          header = header,
                          reduce_labels = reduce_labels,
                          style = style,
                          trigger_button_text = trigger_button_text,
                          allow_na = allow_na,
                          failed_validation_message = failed_validation_message,
                          save_answer = save_answer,
                          hide_response_ui = hide_response_ui,
                          random_order = random_order,
                          response_ui_id = response_ui_id,
                          on_complete = on_complete,
                          admin_ui = admin_ui,
                          instruction = instruction2)
}
