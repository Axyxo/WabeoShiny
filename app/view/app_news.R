# bapp_news
box::use(
  shiny[...],
  #shiny.semantic[flow_layout, update_dropdown_input, vertical_layout],
  #bslib[card,card_header,layout_sidebar,sidebar]
  #plotly[plotly]
  #shiny.fluent[Text]
)

box::use(
  
  app/logic/utils[makeCard,makeCard2,makeCardwithgear,makeCardwithsidebar
                  ,make_numeric_input],
  
  #app/logic/utils[make_directory_text_input_fluent],
  #app/view/layout/layout
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # makePagelayout(id,
  makeCard2("App News",  small = TRUE, center = TRUE,
  div(
    style = "display: flex; justify-content: center; align-items: block; height: 80vh; flex-direction: column;",
    #h3(a("WABEO App News")),
    #br(),
    fluidRow(
      column(3, strong("Datum")),
      column(6, strong("Beschreibung"))
    ),
    fluidRow(
      column(3, "0x-0x-2025"),
      column(6, "nächste Verbesserungen: --> Anzeige von Daten " )
    ),
    fluidRow(
      column(3, "02-03-2025"),
      column(6, "erste Version. Datengrundlagen können gewechselt werden. R6 Variablen, Shiny app. Appsilon Rhino app struktur... " )
    ),
    
  )
  )
}