box::use(
  R6[R6Class],
  shiny[reactiveValues]
)

#' @export
btw25daten <- R6::R6Class(classname = "btw25daten",
  public = list(
    triggers = reactiveValues(plot = 0),
    trigger_plot = function() {
      self$triggers$plot <- self$triggers$plot + 1
    },
    
    #Directory <- "app//static//",

    
    Bayern =  load("app/static/Bayern.RData"),
    
    
    
    set_vars = function(Bayern
                        ) {
      self$Bayern <- Bayern
    }
  )
  )

