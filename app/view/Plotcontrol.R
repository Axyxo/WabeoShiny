
# Plotcontrol
box::use(
  shiny[moduleServer, observeEvent, NS, reactive, is.reactive, h4, div,tags,tagList,reactiveVal,selectInput,checkboxInput,sliderInput
        ,plotOutput,fluidPage,fluidRow,column,HTML,htmlOutput,renderPlot,actionButton,renderText,verbatimTextOutput
        ,updateSelectInput,selectizeInput,updateSelectizeInput,icon,numericInput],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[bs_theme,page_fillable,font_google,page_navbar,nav_panel,navset_pill_list,navset_pill,
        card,card_header,layout_sidebar,sidebar,nav_menu,nav_item,navset_card_pill,nav_spacer,navset_underline,card_body],
  plotly[...],
  # stringr[str_split],
  RColorBrewer[brewer.pal],
  # grDevices[colorRampPalette]
)

box::use(
  app/logic/utils[make_numeric_input],
  #                 ,makeCard,make_numeric_input_fluent
  #                 ,make_PrimaryButton_fluent,make_dropdown_fluent,make_numeric_input],
  # 
  # app/logic/AnsysFunctions/getResultsFromDirectory[getResultsFromDirectory],
  # app/logic/sort_points[sort_points]
  app/logic/constants[Sequential_palette,Qualitative_palette,Diverging_palette]
  
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # tags$head(tags$style(HTML("div#inline label { width: 32%; }
  #                              div#inline input { display: inline-block; width: 68%;}")))
  # tags$head(
  #   tags$style(type="text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; }
  #                                  #inline .form-group { display: table-row;}"))
  
  div(
    #id="inline",  style="width:35vw;",
    # selectInput(ns("funkname"), label = "Funktion"
    #             ,choices = c("mean"
    #                          ,"median"
    #                          ,"std.abw"
    #                          ,"Varianz"
    #                          ,"25% Quantile"
    #                          ,"50% Quantile"
    #                          ,"75% Quantile"
    #             )
    #             ,selected="median"
    # )
    selectInput(ns("AutoColor"), "Autocolor", c("Auto Color" = "TRUE", "manual Color" = "FALSE")
                
    )
    
    ,selectInput(inputId = ns("ColorPalette"), 
                 label = "ColorPalette", 
                 choices = list(
                   Sequential_palettes = Sequential_palette(),
                   Qualitative_palettes = Qualitative_palette(),
                   Diverging_palettes = Diverging_palette()
                 )
                 ,selected="RdYlBu"
                 #, options = list(`actions-box` = TRUE), 
                 ,multiple = FALSE)
    
    #make_numeric_input(inputId = ns("start_value"),label="start Value for all",  description = "start value for all Variables"),
    #well()
    ,make_numeric_input(ns("MAX"),label = "max. Value to plot",description = "max. Value to plot"
                        , defaultValue = 15, min = 0, max = 20, step = 1
    )
    ,make_numeric_input(ns("MIN"), label = "min. Value to plot", defaultValue = -15, min=-20, max=0, step=1
    )
    ,make_numeric_input(ns("ncolor"), label = "number of colors", defaultValue = 12, min = 1, max = 50, step = 2
    )
    ,make_numeric_input(ns("TitleFontSize"), label = "Title Font Size", defaultValue = 14, min = 6, max = 70, step = 1
    )
    ,make_numeric_input(ns("AxisFontSize"), label = "Axis Font Size", defaultValue = 10, min = 6, max = 30, step = 1
    )
    # ,numericInput(ns("Imagewidth"), "Plot width", 800, min=300, max=2048, step=100)
    # ,numericInput(ns("Imagehight"), "Plot hight", 600, min=300, max=2048, step=100)
    ,sliderInput(ns("DecimalPlaces"), "number of decimal places", 3, min=0, max=8, step=1
    )
    ,sliderInput(ns("linesmoothing"), "Smoothing", 0.8, min=0, max=1.3, step=0.1
    )
    
    ,checkboxInput(ns("showlabels"), "Linelabels?", FALSE
    )
    ,checkboxInput(ns("showlines"), "Lines?", TRUE
    )

  )
}

#' @export
server <- function(id,PlotControlVariables) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(list(input$AutoColor
                      ,input$MAX
                      ,input$MIN
                      ,input$ncolor
                      ,input$TitleFontSize
                      ,input$AxisFontSize
                      # ,input$Imagewidth
                      # ,input$Imagehight
                      ,input$DecimalPlaces
                      ,input$linesmoothing
                      ,input$showlabels
                      ,input$showlines
                      ,input$ColorPalette
                      ),{
                        PlotControlVariables$set_vars(
                          input$AutoColor
                          ,input$MAX
                          ,input$MIN
                          ,input$ncolor
                          ,input$TitleFontSize
                          ,input$AxisFontSize
                          # ,input$Imagewidth
                          # ,input$Imagehight
                          ,input$DecimalPlaces
                          ,input$linesmoothing
                          ,input$showlabels
                          ,input$showlines
                          ,input$ColorPalette
                          )
                        
                          PlotControlVariables$trigger_plot()
                          
                         # reactive(input$ncolor)
                      })
  })
}