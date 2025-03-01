box::use(
  R6[R6Class],
  shiny[reactiveValues]
)

#' @export
ConfigVariables <- R6::R6Class(
  classname = "ConfigVariables",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    
    #browser(),
    
    Ansys_EXE_directory = NULL,
    Ansys_Model_source_directory = NULL,
    ANSYS_Working_Directory = NULL,
    Result_directory = NULL,
    OldResults_Directory = NULL,
    cellresult_file_name = NULL,
    noderesult_file_name = NULL,
    ETableresult_file_name = NULL,
    ETableNodeCoords_file_name= NULL,
    
    x_cells= NULL,
    y_cells= NULL,
    SMC_component_name= NULL,
    Leadframe_component_name= NULL,
    Mold_component_name= NULL,
    DA_component_name= NULL,
    pad_surface_nodes_name= NULL,
    start_value= NULL,
    lowerBound_value= NULL,
    upperBound_value= NULL,
    smallest_substep_divider= NULL,
    Restart_Loadstep= NULL,
    Restart_LS_Time_step= NULL,
    APDL_LOAD_LS= NULL,
    APDL_Endtime_LastLS= NULL,
    
    Output_Components=NULL,
    Output_file_name=NULL,
    InterfaceNames=NULL,
    
    set_vars = function(Ansys_EXE_directory
                        ,Ansys_Model_source_directory
                        ,ANSYS_Working_Directory
                        ,Result_directory
                        ,OldResults_Directory
                        ,cellresult_file_name
                        ,noderesult_file_name
                        ,ETableresult_file_name
                        ,ETableNodeCoords_file_name 
                        ,x_cells
                        ,y_cells
                        ,SMC_component_name
                        ,Leadframe_component_name
                        ,Mold_component_name
                        ,DA_component_name
                        ,pad_surface_nodes_name
                        ,start_value
                        ,lowerBound_value
                        ,upperBound_value
                        ,smallest_substep_divider
                        ,Restart_Loadstep
                        ,Restart_LS_Time_step
                        ,APDL_LOAD_LS
                        ,APDL_Endtime_LastLS
    ) {
      
      self$Ansys_EXE_directory <- Ansys_EXE_directory
      self$Ansys_Model_source_directory <- Ansys_Model_source_directory
      self$ANSYS_Working_Directory <- ANSYS_Working_Directory
      self$Result_directory <- Result_directory
      self$OldResults_Directory <- OldResults_Directory
      self$cellresult_file_name <- cellresult_file_name
      self$noderesult_file_name <- noderesult_file_name
      self$ETableresult_file_name <- ETableresult_file_name
      self$ETableNodeCoords_file_name <- ETableNodeCoords_file_name
      self$x_cells <- x_cells
      self$y_cells <-y_cells 
      self$SMC_component_name <-SMC_component_name 
      self$Leadframe_component_name <- Leadframe_component_name
      self$Mold_component_name <- Mold_component_name
      self$DA_component_name <- DA_component_name
      self$pad_surface_nodes_name <- pad_surface_nodes_name
      self$start_value <- start_value
      self$lowerBound_value <-lowerBound_value 
      self$upperBound_value <- upperBound_value
      self$smallest_substep_divider <-smallest_substep_divider 
      self$Restart_Loadstep <- Restart_Loadstep
      self$Restart_LS_Time_step <- Restart_LS_Time_step
      self$APDL_LOAD_LS <- APDL_LOAD_LS
      self$APDL_Endtime_LastLS <- APDL_Endtime_LastLS
      
      # ElementNodeCoords_file_name<-paste("_CoordsEtable_",Componentname)
      # ETableresult_file_name<-paste("Etable_",Componentname)
      # Noderesult_file_name<-paste("Nodes_",Componentname)
      # Cellresult_file_name<-paste("Cellresult_",Componentname)
      
      self$cellresult_file_name <- cellresult_file_name
      self$noderesult_file_name <- noderesult_file_name
      self$ETableresult_file_name <- ETableresult_file_name
      self$ETableNodeCoords_file_name <- ETableNodeCoords_file_name
      
      InterfaceNames<-c("_SMC","_Mold-LF-interface")
      self$InterfaceNames<-InterfaceNames
      self$Output_Components<-data.frame(component=c(SMC_component_name,Mold_component_name)    # important for the creation of the file.ans main input file
                                         ,minormax=c(1,0) #it the top or at the bottom of the component
                                         ,WithCellresult=c(1,0) # write a cellresult of this? 
                                         ,InterfaceName=InterfaceNames
      )
      
      self$Output_file_name<-data.frame(
        ElementNodeCoords_file_name=c(paste(ETableNodeCoords_file_name,InterfaceNames[1], sep = ""),paste(ETableNodeCoords_file_name,InterfaceNames[2], sep = ""))
        ,ETableresult_file_name=c(paste(ETableresult_file_name,InterfaceNames[1], sep = ""),paste(ETableresult_file_name,InterfaceNames[2], sep = ""))
        ,Noderesult_file_name=c(paste(noderesult_file_name,InterfaceNames[1], sep = ""),paste(noderesult_file_name,InterfaceNames[2], sep = ""))
        ,Cellresult_file_name=c(paste(cellresult_file_name,InterfaceNames[1], sep = ""),paste(cellresult_file_name,InterfaceNames[2], sep = ""))
      )
    }
  )
)



#' @export
PlotControlVariables <- R6::R6Class(
  classname = "PlotControlVariables",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    
    AutoColor= NULL,
    MAX= NULL,
    MIN= NULL,
    ncolor= NULL,
    TitleFontSize= NULL,
    AxisFontSize= NULL,
    # Imagewidth= NULL,
    # Imagehight= NULL,
    DecimalPlaces= NULL,
    linesmoothing= NULL,
    showlabels= NULL,
    showlines= NULL,
    ColorPalette= NULL,
    
    set_vars = function(AutoColor,
                            MAX,
                            MIN,
                            ncolor,
                            TitleFontSize,
                            AxisFontSize,
                            # Imagewidth,
                            # Imagehight,
                            DecimalPlaces,
                            linesmoothing,
                            showlabels,
                            showlines,
                            ColorPalette
                            
    ) {
      
      
      self$AutoColor <- AutoColor
      self$MAX <- MAX
      self$MIN <- MIN
      self$ncolor <- ncolor
      self$TitleFontSize <- TitleFontSize
      self$AxisFontSize <- AxisFontSize
      # self$Imagewidth <- Imagewidth
      # self$Imagehight <- Imagehight
      self$DecimalPlaces <- DecimalPlaces
      self$linesmoothing <- linesmoothing
      self$showlabels <- showlabels
      self$showlines <- showlines
      self$ColorPalette <- ColorPalette
      
    }
  )
)


#' @export
PlotResultVariables <- R6::R6Class(
  classname = "PlotResultVariables",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    
    # input$ResultFile
    # ,input$ResultInterface
    # ,input$ResultNset
    # ,input$ResultType
    
    ResultFileName = NULL,
    #ResultInterface = NULL,
    ResultLoadStepLoad = NULL,
    ResultSubStepLoad = NULL,
    ResultLoadStepRef = NULL,
    ResultSubStepRef = NULL,
    #ResultType = NULL,
    Subtraction = NULL,

    
    set_vars = function(ResultFileName
                        #,ResultInterface
                        ,ResultLoadStepLoad
                        ,ResultSubStepLoad
                        ,ResultLoadStepRef
                        ,ResultSubStepRef
                        #,ResultType
                        ,Subtraction
    ) {
      
      self$ResultFileName <- ResultFileName
      #self$ResultInterface <- ResultInterface
      self$ResultLoadStepLoad <- ResultLoadStepLoad
      self$ResultSubStepLoad <- ResultSubStepLoad
      self$ResultLoadStepRef <- ResultLoadStepRef
      self$ResultSubStepRef <- ResultSubStepRef
      #self$ResultType <- ResultType
      self$Subtraction <- Subtraction
    }
  )
)
