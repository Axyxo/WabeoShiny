# btw25_ResultSelector.R
box::use(
  shiny[moduleServer, observeEvent, NS, reactive,actionButton,verbatimTextOutput,selectizeInput,icon,renderText,req,updateSelectizeInput,selectInput],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[sidebar,layout_columns],
  stringr[str_split_i],
  # RColorBrewer[brewer.pal],
  # grDevices[colorRampPalette],
  # reshape2[acast,melt]
)

box::use(
  # app/logic/utils[make_directory_text_input
  #                 ,makeCard,make_numeric_input_fluent
  #                 ,make_PrimaryButton_fluent,make_dropdown_fluent,make_numeric_input],
  app/logic/getbtw25Results[getbtw25Results],
  # app/logic/sort_points[sort_points],
  # app/view/Plotcontrol,
  # app/view/Plot_NodeResult_Contour,
  app/logic/variablesManager[btw25daten],
  #app/logic/simdataManager[SimResults]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebar(
    selectInput(inputId = ns("btw25_region"), label = "Region"
                 ,choices = c("Bayern"
                              ,"Niedersachsen"
                              )
                 ,selected = "Bayern")
    
    ,verbatimTextOutput(ns("loadedoutput"))
    

    
  )}

#' @export
server <- function(id,btw25daten) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$btw25_region, {
      
      filepath<-paste0("app/static/",input$btw25_region ,".RData")
      print(filepath)
      
      load(filepath)
      
      btw25daten$set_vars(data = CSV_Data
      )
      
      btw25daten$trigger_plot()
      
    })

    output$loadedoutput <- renderText({
      input$btw25_region
      req(btw25daten)
      #SimResults$Node_Files_SMC
      aaa<<-btw25daten
      print(paste(dim(btw25daten$data$data), " Results loaded"))
      
      return(paste(dim(btw25daten$data$data), " Results loaded"))
    })
    
    ################################################
    # SimResults<-Si
    # ConfigVariables<-Co
    # SimResults$
    ################################################

  })}