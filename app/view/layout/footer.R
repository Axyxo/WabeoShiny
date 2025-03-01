box::use(
  #checkmate[assert_string, assert_vector],
  #waiter[waiter_preloader],
  shiny[moduleServer, observeEvent, NS, reactive, is.reactive, h4, div,tags],
  #htmltools[tagList,img,div],
  shiny.fluent[Stack,Text]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  Stack(
    horizontal = TRUE,
    horizontalAlign = 'space-between',
    tokens = list(childrenGap = 20),
    Text(variant = "medium", "Built by  Ingenieurbüro Schindler-Saefkow", block=TRUE),
    Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to ne at florian@schindler-saefkow.de"),
    Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
  )
}

