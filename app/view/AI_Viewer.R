# AI_Viewer
box::use(
  shiny[moduleServer, observeEvent, NS, reactive, is.reactive, h4, div,tags,tagList,reactiveVal,selectInput
        ,plotOutput,fluidPage,fluidRow,column,HTML,htmlOutput,renderPlot,actionButton,renderText,verbatimTextOutput
        ,updateSelectInput,selectizeInput,updateSelectizeInput,icon,req,checkboxInput,bindEvent,isolate],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[bs_theme,page_fillable,font_google,page_navbar,nav_panel,navset_pill_list,navset_pill,
        card,card_header,layout_sidebar,sidebar,nav_menu,nav_item,navset_card_pill,nav_spacer,navset_underline,card_body,layout_columns,layout_column_wrap],
  plotly[...],
  
  RColorBrewer[brewer.pal],
  grDevices[colorRampPalette]
)

box::use(
  app/logic/utils[make_directory_text_input
                  ,makeCard,make_numeric_input_fluent
                  ,make_PrimaryButton_fluent,make_dropdown_fluent,make_numeric_input],
  
  #  app/logic/AnsysFunctions/getResultsFromDirectory[getResultsFromDirectory],

  app/view/Plotcontrol,
  app/view/Ai_view_ResultSelector,
  #app/view/Plot_NodeResult_Contour,
  app/logic/variablesManager[PlotControlVariables,PlotResultVariables],
  app/logic/simdataManager[SimResults]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  # navset_pill(
  #   nav_panel(title = "Etable",
  
  card(
    #height = "100%",
    #min_height = "800px",
    #full_screen = TRUE,
    # div(
    #style = "display: flex; justify-content: center; align-items: center; height: 80vh;",
    # h4("Ai Viewer"),#, class = "px-3 my-3")
    #card_header("Ai Viewer"),
    card_body(
      layout_sidebar(
        
        sidebar = Ai_view_ResultSelector$ui(ns("ResultSelector"))
        ,layout_column_wrap(width = 1/2
                            #,Plot_NodeResult_Contour$ui(ns("Elementresultplot"),"ggplot") 
                            #,Plot_NodeResult_Contour$ui(ns("Noderesultplot"),"plotly")
                            #,Plot_NodeResult_Contour$ui(ns("Noderesultplot2"),"plotly")
                            #,Plot_NodeResult_Contour$ui(ns("Noderesultplot3"),"plotly")
        )
        
        #,border_radius = FALSE,
        #fillable = TRUE
        #,class = "p-0"
      ))
  )
  #   )
  # )
  
  #,nav_panel(title = "chip")
  
}

# ConfigVariables
# ConfigVariables,ConfigVariables,SimResults
#' @export
server <- function(id, ConfigVariables) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    SimResults <- SimResults$new()
    PlotResultVariables <- PlotResultVariables$new()
    
    Ai_view_ResultSelector$server("ResultSelector",ConfigVariables,PlotResultVariables,SimResults)
    
    #Plot_NodeResult_Contour$server("Elementresultplot", ConfigVariables,PlotResultVariables,SimResults,3,"Element")
    #Plot_NodeResult_Contour$server("Noderesultplot", ConfigVariables,PlotResultVariables,SimResults,1,"Node")
    #Plot_NodeResult_Contour$server("Noderesultplot2", ConfigVariables,PlotResultVariables,SimResults,2,"Node")
    #Plot_NodeResult_Contour$server("Noderesultplot3", ConfigVariables,PlotResultVariables,SimResults,3,"Node")

    
    # ##########################################
    Co <<- ConfigVariables
    Si <<- SimResults
    PlRe <<- PlotResultVariables
    #PlCo <<- PlotControlVariables
    ###########################################
    
    
  })
}

