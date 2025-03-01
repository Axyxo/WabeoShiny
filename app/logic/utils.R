box::use(
  checkmate[assert_string, assert_vector],
  waiter[waiter_preloader],
  shiny[img, div, br, tagList,textInput,numericInput,tags],
  #shiny.semantic[dropdown_input,numericInput],
  shiny.fluent[TextField.shinyInput,fluentPage,Stack,Text,SpinButton.shinyInput,PrimaryButton.shinyInput,Dropdown.shinyInput],
  glue[glue],
  bslib[card,card_header, card_body,layout_sidebar,sidebar,navset_card_tab,nav_panel, 
        nav_spacer, nav_menu, nav_item,tooltip,popover],
  
  bsicons[bs_icon]
)
box::use(
  #app/view/layout/footer,
  #app/view/layout/header,
  #app/view/layout/navigation,
)


#' @export
loading_screen <- function(text = "Loading...", bkg_color = "white") {
  assert_string(text, min.chars = 1)
  assert_string(bkg_color, min.chars = 1)
  waiter_preloader(
    html = tagList(
      img(src = "static/logo_transparent.png", width = "350"),
      div(class = "load-text", text)
    ),
    color = bkg_color
  )
}

#' @export
make_PrimaryButton_fluent <- function(inputId,text,toggle =TRUE) {
  assert_string(inputId, min.chars = 1)
  assert_string(text, min.chars = 1)
  #assert_string(description, min.chars = 1)
  div(
    class = "PrimaryButton",
    # updatePrimaryButton.shinyInput(
    #   session = shiny::getDefaultReactiveDomain(),
    #   inputId,
    #   ...
    # )
    
    PrimaryButton.shinyInput(
      inputId=inputId,
      text  = text ,
      toggle =toggle,
    )
  )
}
#' @export
make_numeric_input <- function(inputId,label,description,min=0.0001,max=10e9,defaultValue=0 ,step=1) {
  description <- label
  assert_string(inputId, min.chars = 1)
  assert_string(label, min.chars = 1)
  assert_string(description, min.chars = 1)
  div(
    # class = "numericInput",
    tags$table(
      tags$tr(width = "100%",
              tags$td(width = "60%", div(style = "font-size:14px;", label)),
              tags$td(width = "40%", numericInput(inputId=inputId,
                                                  label = NULL,
                                                  value=defaultValue,
                                                  #defaultValue  = defaultValue ,
                                                  step = step,
                                                  min=min,
                                                  max=max
                                                  ,width = "100%"
              )))) |>
      tooltip(description)
  )
}

#' @export
make_numeric_input_fluent <- function(inputId,label,description,min=0.0001,max=10e9,defaultValue=0 ,step=1,precision=4) {
  assert_string(inputId, min.chars = 1)
  assert_string(label, min.chars = 1)
  assert_string(description, min.chars = 1)
  div(
    class = "numericInput",
    SpinButton.shinyInput(
      inputId=inputId,
      label = label,
      value=defaultValue,
      defaultValue  = defaultValue ,
      title = description,
      precision=precision,
      step = step,
      min=min,
      max=max
    )
  )
}

#' @export
make_directory_text_input_fluent <- function(inputId,label,description,defaultValue) {
  assert_string(inputId, min.chars = 1)
  assert_string(label, min.chars = 1)
  assert_string(description, min.chars = 1)
  div(
    #class = "directoryTextInput",
    TextField.shinyInput(
      inputId=inputId,
      label = label,
      #type = "text",
      description = description,
      value = defaultValue
    )
  )
}

#' @export
make_directory_text_input <- function(inputId,label,  description,defaultValue) {
  assert_string(inputId, min.chars = 1)
  assert_string(label, min.chars = 1)
  assert_string(description, min.chars = 1)
  div(
    #class = "directoryTextInput",
    textInput(
      inputId=inputId
      ,label  = label
      ,value= defaultValue
      ,width = "100%"
    )|>
      tooltip(description)
  )
}

#' @export
make_dropdown <- function(input_id, text, choices) {
  assert_string(input_id, min.chars = 1)
  assert_string(text, min.chars = 1)
  assert_vector(
    x = choices,
    strict = TRUE,
    all.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    null.ok = TRUE
    # `null.ok = FALSE` breaks the app, but there's no docs so it's hard to
    # understand if accepting NULL is desired behaviour or some missing req()
  )
  
  div(
    class = "column",
    dropdown_input(
      input_id = input_id,
      default_text = text,
      type = "fluid search selection",
      choices = choices
    )
  )
}

#' @export
make_dropdown_fluent <- function(inputId, label , value, options) {
  # options <- list(
  #   list(key = "A", text = "Option A"),
  #   list(key = "B", text = "Option B"),
  #   list(key = "C", text = "Option C")
  # )
  assert_string(inputId, min.chars = 1)
  
  div(
    class = "column",
    Dropdown.shinyInput(
      inputId = inputId,
      label  = label,
      multiSelect = FALSE,
      #  type = "fluid search selection",
      options = options,
      value=value
    )
  )
}

#' @export
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

#' @export
makeCard_fluent <- function(title, content, size = 6, style = "") {
  div(class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
      style = style,
      Stack(
        tokens = list(childrenGap = 5),
        Text(variant = "large", title, block = TRUE),
        content
      ))
}

#' @export
makeCardwithgear <- function(title,  content, headercontent = NULL, min_height = NULL, gear = NULL) {
  # div(
  card(full_screen = TRUE,#fill = TRUE,#height  = 850,
       card_header(title, headercontent, class = "d-flex justify-content-between",
                   popover(
                     bs_icon("gear"),
                     gear,
                     title = "controls"
                   )
       ) #,class = "d-flex justify-content-between")
       ,card_body(min_height = min_height,
                  content
       )
  )
  # )
}

#' @export
makeCardwithsidebar <- function(title,  content, headercontent = NULL, min_height = NULL, gear = NULL) {
  div(
    card(full_screen = FALSE,fill = TRUE,#height = 250,
         card_header(title, headercontent, class = "d-flex justify-content-between"
         ) #,class = "d-flex justify-content-between")
         ,card_body(min_height = min_height,
                    layout_sidebar(
                      sidebar = sidebar(gear, position = "right", open = FALSE)
                      ,content
                    )
         )
    )
  )
}

#' @export
makeCard <- function(title,  content, headercontent = NULL, min_height = NULL) {
  div(
    card(full_screen = FALSE,fill = TRUE,#height = 250,
         card_header(title, headercontent,class = "d-flex justify-content-between"
         ) #,class = "d-flex justify-content-between")
         ,card_body(min_height = min_height,
                    content
         )
    )
  )
}
