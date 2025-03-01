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
  app/logic/AnsysFunctions/getResultsFromDirectory[getResultsFromDirectory],
  # app/logic/sort_points[sort_points],
  # app/view/Plotcontrol,
  # app/view/Plot_NodeResult_Contour,
  app/logic/variablesManager[PlotResultVariables],
  app/logic/simdataManager[SimResults]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebar(
    actionButton(ns("LoadResultdirectory")
                 , label  = "Load results"
                 , icon = icon("cloud-upload")
    )
    
    ,verbatimTextOutput(ns("CountOfLoadedResults"))
    
    ,selectizeInput(inputId = ns("ResultFileName"), label = "resultfile:",choices = "nothing selected", width = "100%",
                    multiple = FALSE
                    ,options = list(
                      #create = FALSE,
                      placeholder = "Search Me",
                      #maxItems = '1',
                      #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                      onType = I("function (str) {if (str === \"\") {this.close();}}"),
                      deselectBehavior = "previous"
                    )
    )
    
    # ,selectizeInput(inputId = ns("ResultInterface"), label = "Interface:",choices = "nothing selected", width = "100%",
    #                 multiple = FALSE
    #                 ,options = list(
    #                   #create = FALSE,
    #                   placeholder = "Search Me",
    #                   #maxItems = '1',
    #                   #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
    #                   onType = I("function (str) {if (str === \"\") {this.close();}}"),
    #                   deselectBehavior = "previous"
    #                 )
    # )
    # 
    # ,selectizeInput(inputId = ns("ResultType"), label = "resulttype:",choices = "nothing selected", width = "100%",
    #                 multiple = FALSE
    #                 ,options = list(
    #                   #create = FALSE,
    #                   placeholder = "Search Me",
    #                   #maxItems = '1',
    #                   #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
    #                   onType = I("function (str) {if (str === \"\") {this.close();}}"),
    #                   deselectBehavior = "previous"
    #                 ))
    
    ,selectInput(inputId = ns("Subtraction"), label = "Subtraction?"
                 ,choices = c("Load minus Ref"
                              ,"Reference only"
                              ,"Load only")
                 ,selected = "Load minus Ref")
    
    ,layout_columns(
      selectizeInput(inputId = ns("ResultLoadStepRef"), label = "LoadStep Ref:",choices = "nothing selected", width = "100%",
                     multiple = FALSE
                     ,options = list(
                       #create = FALSE,
                       placeholder = "Search Me",
                       #maxItems = '1',
                       #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}"),
                       deselectBehavior = "previous"
                     ))
      ,selectizeInput(inputId = ns("ResultSubStepRef"), label = "SubStep Ref:",choices = "nothing selected", width = "100%",
                      multiple = FALSE
                      ,options = list(
                        #create = FALSE,
                        placeholder = "Search Me",
                        #maxItems = '1',
                        #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                        onType = I("function (str) {if (str === \"\") {this.close();}}"),
                        deselectBehavior = "previous"
                      ))
    )
    ,layout_columns(
      selectizeInput(inputId = ns("ResultLoadStepLoad"), label = "LoadStep Load:",choices = "nothing selected", width = "100%",
                     multiple = FALSE
                     ,options = list(
                       #create = FALSE,
                       placeholder = "Search Me",
                       #maxItems = '1',
                       #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       onType = I("function (str) {if (str === \"\") {this.close();}}"),
                       deselectBehavior = "previous"
                     ))
      ,selectizeInput(inputId = ns("ResultSubStepLoad"), label = "SubStep Load:",choices = "nothing selected", width = "100%",
                      multiple = FALSE
                      ,options = list(
                        #create = FALSE,
                        placeholder = "Search Me",
                        #maxItems = '1',
                        #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                        onType = I("function (str) {if (str === \"\") {this.close();}}"),
                        deselectBehavior = "previous"
                      ))
      
    )
  )}

#' @export
server <- function(id,ConfigVariables, PlotResultVariables,SimResults) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ################################################
    # SimResults<-Si
    # ConfigVariables<-Co
    # SimResults$
    ################################################

    observeEvent(
      list(input$ResultFileName
           #,input$ResultInterface
           ,input$ResultLoadStepLoad
           ,input$ResultSubStepLoad
           ,input$ResultLoadStepRef
           ,input$ResultSubStepRef
           #,input$ResultType
           ,input$Subtraction
           
      ),{
        #print("1231234observeEvent(")
        PlotResultVariables$set_vars(input$ResultFileName
                                     #,input$ResultInterface
                                     ,input$ResultLoadStepLoad
                                     ,input$ResultSubStepLoad
                                     ,input$ResultLoadStepRef
                                     ,input$ResultSubStepRef
                                     #,input$ResultType
                                     ,input$Subtraction
        )
        PlotResultVariables$trigger_plot()
      })
    
    observeEvent(input$LoadResultdirectory, {
      
      Allresults <- getResultsFromDirectory(ConfigVariables)
      
      InterfaceList <- ConfigVariables$InterfaceNames
      Nodelist <- Allresults[[1]]$Noderesult
      Etablelist <- Allresults[[1]]$Etableresult
      
      NodeResultFileList <- unique(dimnames(Nodelist)[[3]])
      NodeResultNameList <- str_split_i(NodeResultFileList, ConfigVariables$Output_file_name$Noderesult_file_name[1], 1)
      names(NodeResultFileList) <- NodeResultNameList
      
      EtableResultFileList <- unique(dimnames(Etablelist)[[3]])
      EtableResultNameList <- str_split_i(EtableResultFileList, ConfigVariables$Output_file_name$ETableresult_file_name[1], 1)
      names(EtableResultFileList) <- EtableResultNameList
      
      NodeResultTypeList <- dimnames(Nodelist)[[2]]
      EtableResultTypeList <- dimnames(Etablelist)[[2]]
      #resultnamesoneIF <- resultnames[grepl(ConfigVariables$Output_file_name$Noderesult_file_name[1],resultnames, fixed = TRUE)]
      
      Nodeone <- as.data.frame(Nodelist[,,1])
      LoadStepList <- (unique(Nodeone$Loadstep))
      SubStepList <- (unique(Nodeone$Substep))
      
      
      SimResults$set_vars(all <- Allresults
                          ,NodeResultFileList = NodeResultFileList
                          ,NodeResultNameList = NodeResultNameList
                          ,EtableResultFileList = EtableResultFileList
                          ,EtableResultNameList = EtableResultNameList
                          ,InterfaceList = InterfaceList
                          ,NodeResultTypeList = NodeResultTypeList
                          ,EtableResultTypeList = EtableResultTypeList
                          ,LoadStepList = LoadStepList
                          ,SubStepList = SubStepList
      )
      
      SimResults$trigger_plot()
      
      updateSelectizeInput(inputId = "ResultFileName",choices = NodeResultNameList ,selected = NodeResultNameList[1])
      # updateSelectizeInput(inputId = "ResultInterface",choices = InterfaceList ,selected = InterfaceList[1]) 
      # updateSelectizeInput(inputId = "ResultType",choices = NodeResultTypeList,selected = "Sx")
      
      updateSelectizeInput(inputId = "ResultLoadStepRef",choices = LoadStepList,selected = LoadStepList[1])
      updateSelectizeInput(inputId = "ResultSubStepRef",choices = SubStepList,selected = SubStepList[length(SubStepList)])
      
      updateSelectizeInput(inputId = "ResultLoadStepLoad",choices = LoadStepList,selected = LoadStepList[length(LoadStepList)])
      updateSelectizeInput(inputId = "ResultSubStepLoad",choices = SubStepList,selected = SubStepList[length(SubStepList)])
    })
    
    # observeEvent(input$ResultInterface, {
    #   
    # })
    
    output$CountOfLoadedResults <- renderText({
      input$LoadResultdirectory
      req(SimResults)
      #SimResults$Node_Files_SMC
      print(paste(dim(SimResults$all[[1]]$Noderesult)[3], " Results loaded"))
      
      return(paste(dim(SimResults$all[[1]]$Noderesult)[3], " Results loaded"))
    })
    
  })}