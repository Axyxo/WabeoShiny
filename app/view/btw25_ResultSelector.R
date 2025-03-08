# btw25_ResultSelector.R
box::use(
  shiny[moduleServer, observeEvent, NS, reactive,actionButton,verbatimTextOutput
        ,selectizeInput,icon,renderText,req,updateSelectizeInput,selectInput,updateSelectInput ],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[sidebar,layout_columns],
  # RColorBrewer[brewer.pal],
  # grDevices[colorRampPalette],
  # reshape2[acast,melt]
)

box::use(
  # app/logic/utils[make_directory_text_input
  #                 ,makeCard,make_numeric_input_fluent
  #                 ,make_PrimaryButton_fluent,make_dropdown_fluent,make_numeric_input],
  #app/logic/getbtw25Results[getbtw25Results],
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
server <- function(id,btw25daten_instance) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$btw25_region, {
      
      filepath<-paste0("app/static/",input$btw25_region ,".RData")
      #print(filepath)
      
      env <- new.env()  # Erstellt eine eigene Umgebung für das Laden
      load(filepath, envir = env)  # Lädt CSV_Data in diese Umgebung
      
      btw25daten_instance$set_vars(data = env$CSV_Data)  # Zugriff auf CSV_Data in env
      btw25daten_instance$trigger_plot()  # Aktualisierung auslösen
      
      })
    
    # observeEvent(input$BTWcolnames, {
    #   
    #   btw25daten$set_vars(BTWcolname = input$BTWcolname)
    #   
    # })
    
    output$loadedoutput <- renderText({
      
      input$btw25_region
      req(btw25daten_instance$data)
      req(btw25daten_instance$triggers$plot)
      
      #SimResults$Node_Files_SMC
    #  aaa<<-btw25daten
      #print(paste(dim(btw25daten$data$data), " Results loaded"))
      #print(paste(dim(btw25daten$BTWcolnames), " Results loaded"))
      
      return(paste(dim(btw25daten_instance$data$data), " Results loaded"))
    })
    
    ################################################
    # SimResults<-Si
    # ConfigVariables<-Co
    # SimResults$
    ################################################

  })}