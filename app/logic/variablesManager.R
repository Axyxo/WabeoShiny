box::use(
  R6[R6Class],
  shiny[reactiveValues]
)

#' @export
btw25daten <- R6::R6Class(classname = "PlotControlVariables",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    directory <- "C://WABEO//btw25//downloads//"
    load(file = paste0(directory,"/","btw25daten",".RData" ))
    AutoColor =  load,
    
    set_vars = function(AutoColor,

    ) {
      self$AutoColor <- AutoColor
    }
  )
  )

