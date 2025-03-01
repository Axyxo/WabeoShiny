box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,h1,h4
        ,reactiveValues,observe,observeEvent,textOutput,a,actionButton,icon],
  #shiny.semantic[grid, grid_template, semanticPage,tabset,menu,menu_item,menu_divider,dropdown_menu,menu_header,icon],
  waiter[use_waiter,waiter_hide],
  bslib[bs_theme,page_fillable,page_fluid,page_fixed,font_google,page_navbar,nav_panel,navset_pill_list,
        card,card_header,layout_sidebar,sidebar,nav_menu,nav_item,navset_card_pill,nav_spacer,bs_theme_preview,input_dark_mode],
  bsicons[bs_icon],
  #shiny.fluent[fluentPage,Nav],
  #shiny.router[route_link,router_ui,route,router_server]
  #shinyjs[useShinyjs]
)

box::use(
  app/view/root,
  #app/view/secondPage,
  #app/view/conductivityMAIN,
  app/view/btw25_Viewer,
  #app/view/AI_Creator_bslib,
  app/logic/utils[loading_screen,make_directory_text_input
                  ,makeCard
                  #,make_numeric_input_fluent
                  #,make_PrimaryButton_fluent,make_dropdown_fluent
                  ,make_numeric_input],
  #app/view/interactivecellplot,
  #app/view/layout/layout,
  #app/view/layout/navigation,
  app/view/layout/footer,
  app/view/layout/header,
  app/logic/variablesManager[ConfigVariables,PlotResultVariables],
  
  #app/view/interactivecellplot,
  #app/view/namesettings,
  
  #app/logic/sort_points[sort_points]
)

bslib_Wabeo <- bslib::bs_theme(
  # bootswatch = "bootstrap",
  version = 5, 
  # bootswatch = "minty",
  #preset = "sandstone",
  #preset = "minty",
  preset = "bootstrap",
  #bootswatch = "simplex",
  #preset="Bosch",
  
  bg = "#ffffff",
  fg = "#000000",
  primary = "#222c58",
  secondary = "#e4067e",#328d46
  success = "#e475ba",#d42027
  info = "#63a4e6",
  warning = "#73013f",
  danger = "#73013f",
  `enable-gradients` = TRUE,
  `enable-shadows` = TRUE,
  `enable-transitions` = TRUE,
  `enable-rounded` =FALSE,
  spacer = "0.0rem",
  font_scale = 0.9,
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Mono"),
  code_font = font_google("IBM Plex Mono")
)

bslib_BlueMustang<- bslib::bs_theme(
  # bootswatch = "bootstrap",
  version = 5, 
  # bootswatch = "minty",
  #preset = "sandstone",
  #preset = "minty",
  preset = "bootstrap",
  #bootswatch = "simplex",
  #preset="Bosch",
  
  bg = "#ffffff",
  fg = "#000000",
  primary = "#1a4853",
  secondary = "#518a8b",#328d46
  success = "#dcd3a6",#d42027
  info = "#dcd3a6",
  warning = "#deb887",
  danger = "#633b3b",
  `enable-gradients` = TRUE,
  `enable-shadows` = TRUE,
  `enable-transitions` = TRUE,
  `enable-rounded` =FALSE,
  spacer = "0.0rem",
  font_scale = 0.9,
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Mono"),
  code_font = font_google("IBM Plex Mono")
)


link_shiny <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")

#' @export
ui <- function(id) {
  ns <- NS(id)
  #bootstrapPage(
  #semanticPage(
  # fluentPage(
  #page_fixed(
  page_fillable(
    #loading_screen("StressChip.de"),
    # use_waiter(),
    title = "wabeo.de",
    theme = bslib_Wabeo,
    padding = 0,
    fillable = TRUE,
    # window_title="AI StressChip.de APP",
    # h4("Stresschip.de APP Version 0.01", class = "px-3 my-3"),
    #input_dark_mode(),
    navset_card_pill(
      #padding=0,
      #placement = "above",
      # header = header$ui(ns("irgendwasHeader")),
      # footer = header$ui(ns("irgendwasHeader")),
      
      #nav_panel(title = "Stresschip.de Version 0.01") ,
      
      #navset_pill_list(widths = c(2, 10),#header = header$ui(ns("irgendwasHeader")),
      # well=FALSE,
      # fluid=FALSE),
      title = h4("WABEO Shiny App 0.01"),
      #nav_spacer(),
      nav_panel(title = "WABEO Home", root$ui("root"),icon=bs_icon("house", size = "1em", class = "text-primary")),
      #nav_panel(title = "WABEO Auswertung", namesettings$ui(ns("mainnames")), icon=bs_icon("gear", size = "1em", class = "text-primary")),
      #nav_panel(title = "AI Creator", AI_Creator_bslib$ui(ns("AI_Creator_bslib")),icon=bs_icon("Calculator", size = "1em", class = "text-primary")),
      nav_panel(title = "WABEO Viewer", btw25_Viewer$ui(ns("btw25_Viewer")),icon=bs_icon("eye", size = "1em", class = "text-primary"),fluid = TRUE),
      nav_spacer(),
      nav_menu(
        title = "Links",
        nav_item(a(href="http://wabeo.de" ,target="_blank" ,rel="noopener noreferrer", "wabeo.de")),
        nav_item(a(href="http://schindler-saefkow.de" ,target="_blank" ,rel="noopener noreferrer", "schindler-saefkow.de")),
        nav_item(a(href="https://shiny.posit.co/" ,target="_blank" ,rel="noopener noreferrer", "shiny")),
        nav_item(a(href="https://plotly.com/r/" ,target="_blank" ,rel="noopener noreferrer", "Plotly R Open Source Graphing Library")),
        nav_item(a(href="https://rstudio.github.io/bslib/" ,target="_blank" ,rel="noopener noreferrer", "bslib R package")),
        nav_item(a(href="https://r-graph-gallery.com/38-rcolorbrewers-palettes.html" ,target="_blank" ,rel="noopener noreferrer", "R Color Brewers palettes")),
        nav_item(a(href="https://ggplot2.tidyverse.org/" ,target="_blank" ,rel="noopener noreferrer", "ggplot2")),
        nav_item(a(href="https://nlopt.readthedocs.io/en/latest/" ,target="_blank" ,rel="noopener noreferrer", "NLopt library for nonlinear optimization")),
        nav_item(a(href="https://www.htmlwidgets.org/showcase_metricsgraphics.html" ,target="_blank" ,rel="noopener noreferrer", "htmlwidgets for R")),
        nav_item(a(href="https://rhinoverse.dev/#rhino" ,target="_blank" ,rel="noopener noreferrer", "rhinoverse")),
        nav_item(link_shiny),
        nav_item(link_posit)
        
      )
      ,nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #browser()
    Sys.sleep(0.1)
    waiter_hide()
    
    print(getwd())
    
    #namesettings$server("mainnames", ConfigVariables)
    
    ConfigVariables <- ConfigVariables$new()
    
    #SimResults<-SimResults$new()
    
    ############################################
    #Co<<-ConfigVariables
    
    ###########################################
    
    #AI_Creator_bslib$server("AI_Creator_bslib",ConfigVariables)
    
    btw25_Viewer$server("btw25_Viewer",ConfigVariables)
    
    
  })
}
