is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.character.vector <- function(x) {
  is.vector(x) && is.character(x)
}

is.character.or.numeric <- function(x) {
  is.character(x) || is.numeric(x)
}

# is.na.or.null <- function(x) {
#   is.na(x) || is.null(x)
# }

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

tdGenerateOptions <- function (inputId, selected, inline, type = "checkbox",
                                choiceNames, choiceValues, session = shiny::getDefaultReactiveDomain()){

  options <- mapply(choiceValues, choiceNames, FUN = function(value, name) {
    inputTag <- shiny::tags$input(type = type, name = inputId, value = value)
    if (value %in% selected)
      inputTag$attribs$checked <- "checked"
    pd <- shiny:::processDeps(name, session)
    if (inline) {
      shiny::tags$td(
        shiny::tags$label(class = paste0(type, "-inline"), inputTag, shiny::span(pd$html, pd$deps)),
        style = "width:50px;line-height:50px;vertical-align:center;border:1px solid black;text-align:center;padding:0px")
    }
    else {
      shiny::div(class = type, shiny::tags$label(inputTag, shiny::span(pd$html, pd$deps)))
    }
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  shiny::span(class = "shiny-options-group", options)
}


tdRadioButtons <- function (inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                             width = NULL, choiceNames = NULL, choiceValues = NULL){
  args <- shiny:::normalizeChoiceArgs(choices, choiceNames, choiceValues)
  selected <- shiny::restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected))
    args$choiceValues[[1]]
  else as.character(selected)
  if (length(selected) > 1)
    stop("The 'selected' argument must be of length 1")
  options <- tdGenerateOptions(inputId, selected, inline, "radio",
                                args$choiceNames, args$choiceValues)
  #browser()
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline)
    divClass <- paste(divClass, "shiny-input-container-inline")
  inputLabel <- shiny:::shinyInputLabel(inputId, label)
  shiny::span(id = inputId,
              style = htmltools::css(width = shiny::validateCssUnit(width)),
              class = divClass,
              role = "radiogroup", `aria-labelledby` = inputLabel$attribs$id,
              inputLabel, options)
}


trigger_button_hack <- function(inputId, label, icon = NULL, width = NULL,
                                enable_after = 0, style = "",
                                ...) {
  checkmate::qassert(enable_after, "N1[0,)")
  inputId <- htmltools::htmlEscape(inputId, attribute = TRUE)
  shiny::tagList(
    shiny::actionButton(
      inputId = inputId, label = label,
      icon = icon, width = width,
      onclick = "alert('Clicked');alert(document.getElementsByName('item_01')[1].checked);trigger_button(this.id);",
      disabled = TRUE,
      style = style,
      ...),
    shiny::tags$script(
      sprintf("setTimeout(function() {
                 document.getElementById('%s').disabled = false;
               }, %i);",
              inputId, round(enable_after * 1e3))
    ))
}
