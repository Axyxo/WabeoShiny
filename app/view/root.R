# 
box::use(
  shiny[moduleServer, observeEvent, NS, reactive, is.reactive, h4, div,tags],
  #shiny.semantic[flow_layout, update_dropdown_input, vertical_layout],
  #bslib[card,card_header,layout_sidebar,sidebar]
  #plotly[plotly]
  #shiny.fluent[Text]
)

box::use(
  app/logic/utils[make_directory_text_input_fluent],
  #app/view/layout/layout
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # makePagelayout(id,
  div(
    style = "display: flex; justify-content: center; align-items: center; height: 80vh;",
    tags$h1(
      tags$a("Welcome to the WABEO viewer")#, href = "https://www.WABEO.de")
      #Text(variant = "xLarge", "Welcome to the stresschipAI app"),
      #)
    )
  )
}