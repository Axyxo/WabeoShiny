box::use(
  #checkmate[assert_string, assert_vector],
  #waiter[waiter_preloader],
  shiny[moduleServer, observeEvent, NS, reactive, is.reactive, h4, div,tags],
  htmltools[tagList,img,div],
  shiny.fluent[CommandBar,CommandBarItem,Text]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    #img(src = "appsilon-logo.png", class = "logo"),
    div(Text(variant = "xLarge", "stresschipAI (Version 0.001)"), class = "title"),
    CommandBar(
      items = list(
        CommandBarItem(
          key = ns("newItem"),
          iconOnly = TRUE,
          text = "WiP Menue",
          cacheKey = "myCacheKey",
          split = TRUE,
          iconProps = list(iconName = "Add"),
          subMenuProps = list(
            items = list(
              CommandBarItem(
                key = ns("emailMessage"),
                text = "Email message",
                iconProps = list(iconName = "Mail"),
                href = "mailto:florian@schindler-saefkow.de" ,target="_blank" ,rel="noopener noreferrer"
              )
              ,CommandBarItem(
                key = ns("schindler-saefkow.de"),
                text = "schindler-saefkow.de",
                iconProps = list(iconName = "Website"),
                href = "http://www.schindler-saefkow.de" ,target="_blank" ,rel="noopener noreferrer"
              )
            )
          )
        )
      #   ,CommandBarItem(
      #     key = ns("upload"),
      #     text = "Upload",
      #     iconProps = list(iconName = "Upload")
      #   ),
      #   CommandBarItem(
      #     key = ns("share"),
      #     text = "Share",
      #     iconProps = list(iconName = "Share")
      #   ),
      #   CommandBarItem(
      #     key = ns("download"),
      #     text = "Download",
      #     iconProps = list(iconName = "Download")
      #   )
       ),
      farItems = list(
        # CommandBarItem(
        #   key = ns("tile"),
        #   text = "Grid view",
        #   ariaLabel = "Grid view",
        #   iconOnly = TRUE,
        #   iconProps = list(iconName = "Tiles")
        # )
        CommandBarItem(
          key = ns("info"),
          text = "Info",
          ariaLabel = "Info",
          iconOnly = TRUE,
          iconProps = list(iconName = "Info")
        )
      ),
      style = list(width = "100%"))
    )
}
