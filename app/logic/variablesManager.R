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
    
    data =  NULL,
    
    set_vars = function(data
                        ) {
      self$data <- data
    }
  )
  )


#' @export
DataLoader <- R6::R6Class("DataLoader",
                          public = list(
                            data = NULL,  # Hier werden die Daten gespeichert
                            
                            initialize = function(file_path) {
                              if (file.exists(file_path)) {
                                temp_env <- new.env()  # Erstelle eine neue Umgebung
                                load(file_path, envir = temp_env)  # Lade die Daten in die Umgebung
                                
                                # Falls mehrere Objekte in der RData sind, nehme das erste
                                obj_names <- ls(temp_env)
                                if (length(obj_names) > 0) {
                                  self$data <- temp_env[[obj_names[1]]]
                                } else {
                                  warning("Die .RData-Datei ist leer.")
                                }
                              } else {
                                warning("Die Datei existiert nicht: ", file_path)
                              }
                            },
                            
                            get_data = function() {
                              return(self$data)  # Gibt die geladenen Daten zurück
                            }
                          )
)

