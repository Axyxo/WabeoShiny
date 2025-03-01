#simdataManager
box::use(
  R6[R6Class],
  shiny[reactiveValues]
)

box::use(
  app/logic/AnsysFunctions/getResultsFromDirectory[getResultsFromDirectory]
)


#' @export
SimResults <- R6::R6Class(
  classname = "SimResults",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    
    #browser(),
    # Directory=NULL
    
    
    # noderesult_Files_IF=NULL
    # ,Etable_Files_IF=NULL
    # ,enumLogicOldList_IF=NULL
    # ,Etablecoords_IF=NULL
    # 
    # ,cellresults_SMC=NULL
    # ,noderesult_Files_SMC=NULL
    # ,Etable_Files_SMC=NULL
    # ,Etablecoords_SMC=NULL
    
    all = NULL
    ,NodeResultFileList = NULL
    ,NodeResultNameList = NULL
    ,EtableResultFileList = NULL
    ,EtableResultNameList = NULL
    ,InterfaceList = NULL
    ,NodeResultTypeList = NULL
    ,EtableResultTypeList = NULL
    ,LoadStepList = NULL
    ,SubStepList = NULL
    
    ,set_vars = function(all
                         ,NodeResultFileList
                         ,NodeResultNameList
                         ,EtableResultFileList
                         ,EtableResultNameList
                         ,InterfaceList
                         ,NodeResultTypeList
                         ,EtableResultTypeList
                         ,LoadStepList
                         ,SubStepList
    ) {
      self$all <- all
      self$NodeResultFileList <- NodeResultFileList
      self$NodeResultNameList <- NodeResultNameList
      self$EtableResultFileList <- EtableResultFileList
      self$EtableResultNameList <- EtableResultNameList
      self$InterfaceList <- InterfaceList
      self$NodeResultTypeList <- NodeResultTypeList
      self$EtableResultTypeList <- EtableResultTypeList
      self$LoadStepList <- LoadStepList
      self$SubStepList <- SubStepList
      }
  )
)
# all <- Allresults
# ,NodeResultFileList = NodeResultFileList
# ,NodeResultNameList = NodeResultNameList
# ,EtableResultFileList = EtableResultFileList
# ,EtableResultNameList = EtableResultNameList
# ,InterfaceList = InterfaceNames
# ,NodeResultTypeList = NodeResultTypeList
# ,EtableResultTypeList = EtableResultTypeList
# ,LoadStepList = LoadStepList
# ,SubStepList = SubStepList