# btw25_TimeLinePlot
box::use(
  shiny[...],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[page_fillable,layout_columns,layout_sidebar,sidebar,font_google],
  plotly[...],
  #ggplot2[...],
  #stringr[str_split],
  RColorBrewer[brewer.pal],
  grDevices[colorRampPalette],
  #reshape2[acast,melt],
  #dplyr[filter]
)

box::use(
  app/logic/utils[makeCard,makeCardwithgear,makeCardwithsidebar
                  ,make_numeric_input],
  app/view/Plotcontrol,
  app/view/empty_plot[empty_plot],
  #app/logic/sort_points[sort_points],
  app/logic/variablesManager[btw25daten,PlotControlVariables],
)

#' @export
ui <- function(id) {   #,PlotGraphType="plotly") {
  ns <- NS(id)
  # if (PlotGraphType == "plotly") {plottypeplot <- plotlyOutput(ns("graph"))}
  # if (PlotGraphType == "ggplot") {plottypeplot <- plotOutput(ns("ggplotgraph"))}
  
  makeCardwithgear(title = "BTW25 Auszählung"
                   ,headercontent = div(class = "d-flex justify-content-start"
                                        ,selectInput(inputId = ns("BTWcolname")
                                                     ,label = "Waehle die Spalte mit den Wahlkreisen."
                                                     ,choices = c("nichts geladen")
                                                     ,selected = "nichts geladen"
                                        )
                   ),
                   content = plotlyOutput(ns("graph")), 
                   min_height = NULL, 
                   gear = Plotcontrol$ui(ns("btw25plotcontrol"))
  )
  
}

#' @export
server <- function(id,btw25daten_instance) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PlotControlVariables <- PlotControlVariables$new()
    Plotcontrol$server("btw25plotcontrol",PlotControlVariables)
    
    AutoColor <- PlotControlVariables$AutoColor
    ncolor <- PlotControlVariables$ncolor
    ColorPalette <- PlotControlVariables$ColorPalette
    DezCount <- PlotControlVariables$DecimalPlaces
    TitleFontSize <- PlotControlVariables$TitleFontSize
    AxisFontSize <- PlotControlVariables$AxisFontSize
    linesmoothing <- PlotControlVariables$linesmoothing
    showlabels <- PlotControlVariables$showlabels
    showlines <- PlotControlVariables$showlines
    showscale <- TRUE
    
    #req(btw25daten_instance$data)
    #req(btw25daten_instance$triggers$plot)
    req(btw25daten_instance$data)
    req(btw25daten_instance$triggers$plot)
    
    updateSelectInput(inputId = ns("BTWcolname")
                # ,label = "Waehle die Spalte mit den Wahlkreisen."
                 ,choices = colnames(btw25daten_instance$data$data)
                 #,selected = "nichts geladen11"
                )
    
    output$graph <- renderPlotly({
      
      PlotControlVariables$triggers$plot
      

      
      print(paste(length(btw25daten_instance$data$data), " Results loaded"))

      
      #SimResults$triggers$plot
      
      # if (is.null(btw25daten$data$data) == TRUE) {
      #   print("no data is loaded")
      #   return(empty_plot("no data is loaded"))
      # }
      return(empty_plot(
        dim(btw25daten_instance$data$data)
        ))
     
    })
  })
  
}