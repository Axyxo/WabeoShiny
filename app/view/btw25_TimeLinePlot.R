# btw25_TimeLinePlot
box::use(
  shiny[...],
  #ggplot2[ggplot,geom_ribbon,aes,ggtitle],
  #stats[rnorm],
  bslib[page_fillable,layout_columns,layout_sidebar,sidebar,font_google],
  plotly[...],
  #ggplot2[...],
  #stringr[str_split],
  RColorBrewer[brewer.pal],
  grDevices[colorRampPalette],
  #reshape2[acast,melt],
  #dplyr[filter]
)

box::use(
  app/logic/utils[makeCard,makeCardwithgear,makeCardwithsidebar
                  ,make_numeric_input],
  app/view/Plotcontrol,
  app/view/empty_plot[empty_plot],
  app/logic/sort_points[sort_points],
  app/logic/variablesManager[PlotControlVariables,PlotResultVariables],
)

#' @export
ui <- function(id,PlotGraphType="plotly") {
  ns <- NS(id)
  if (PlotGraphType == "plotly") {plottypeplot <- plotlyOutput(ns("graph"))}
  if (PlotGraphType == "ggplot") {plottypeplot <- plotOutput(ns("ggplotgraph"))}
  makeCardwithgear(title = "Noderesult", 
                   headercontent =     div(class = "d-flex justify-content-start",
                                           selectizeInput(inputId = ns("ResultInterface"), label = "Interface:",choices = "nothing selected", 
                                                          #width = "100%",
                                                          multiple = FALSE#, width = "100%"
                                                          ,options = list(
                                                            #create = FALSE,
                                                            placeholder = "Search Me",
                                                            #maxItems = '1',
                                                            #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                            onType = I("function (str) {if (str === \"\") {this.close();}}"),
                                                            deselectBehavior = "previous"
                                                          )
                                           )
                                           ,selectizeInput(inputId = ns("ResultType"), label = "resulttype:",
                                                           choices = "nothing selected", multiple = FALSE#, width = "100%"
                                                           ,options = list(
                                                             #create = FALSE,
                                                             placeholder = "Search Me",
                                                             #maxItems = '1',
                                                             #onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                             onType = I("function (str) {if (str === \"\") {this.close();}}"),
                                                             deselectBehavior = "previous"
                                                           )
                                           )),
                   content = plottypeplot, 
                   min_height = NULL, 
                   gear = Plotcontrol$ui(ns("Nodeplotcontrol"))
  )
  
}

#' @export
server <- function(id,ConfigVariables, PlotResultVariables, SimResults, DefaultInterface,PlotType) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    PlotControlVariables <- PlotControlVariables$new()
    Plotcontrol$server("Nodeplotcontrol",PlotControlVariables)
    
    observeEvent(SimResults$triggers$plot, {
      ifl <- c("SMC cell positions",SimResults$InterfaceList)
      
      NRTL <- SimResults$NodeResultTypeList
      ERTL <- c("Delamination state", SimResults$EtableResultTypeList)
      
      
      
      if (PlotType == "Node") {
        updateSelectizeInput(inputId = "ResultInterface",choices = ifl ,selected = ifl[DefaultInterface])
        updateSelectizeInput(inputId = "ResultType",choices = NRTL, selected = "SxminSy")
      }
      
      if (PlotType == "Element") {
        updateSelectizeInput(inputId = "ResultInterface",choices = ifl ,selected = ifl[DefaultInterface] , label = "Element result Interface",)
        updateSelectizeInput(inputId = "ResultType",choices = ERTL, selected = "Delamination state")
      }
      
      # print("SimResults getrigert")
    })
    
    ReactivePlotVariables <- reactive({
      #print(SimResults)
      #req(SimResults)
      
      #print("start plot")
      

      
      
      #PlotResultVariables$
      #################################################################################################
      PlCo <<- PlotControlVariables
      # #################################################################################################
      # ConfigVariables<-Co
      # PlotControlVariables<-PlCo
      # PlotResultVariables<-PlRe
      # SimResults<-Si
      # ResultType <- "Sx"
      # ResultInterface <- "_Mold-LF-interface"
      # PlotType = "Element"
      #################################################################################################
      

      
      # if (is.null(SimResults$InterfaceList) == TRUE) {
      #   return(plot_ly()
      #     #fig = plot_ly()
      #     #fig <- fig %>% layout((title = 'please load any data'))
      #   
      #   
      # )}
      
      ResultFileName <- PlotResultVariables$ResultFileName
      ResultInterface <- input$ResultInterface
      ResultLoadStepLoad <- PlotResultVariables$ResultLoadStepLoad
      ResultSubStepLoad <- PlotResultVariables$ResultSubStepLoad
      ResultLoadStepRef <- PlotResultVariables$ResultLoadStepRef
      ResultSubStepRef <- PlotResultVariables$ResultSubStepRef
      ResultType <- input$ResultType
      Subtraction <- PlotResultVariables$Subtraction
      print("Subtraction")
      print(Subtraction)
      #PlRe$Subtraction
      #Noderesult_file_name<-ConfigVariables$Output_file_name$Noderesult_file_name[IFindex]
      
      
      
      #Noderesulttypes<-dimnames(Nodelist)[[2]]
      #Noderesulttypes<-dimnames(Nodelist)[[2]]
      # a1 <- SimResults$all[[1]]$Noderesult
      # a2 <- SimResults$all[[2]]$Noderesult
      
      #cellresult <- TRUE
      #aaa<-SimResults$all
      
      if (ResultInterface == "SMC cell positions") {
        Nodelist <- SimResults$all[["_SMC"]][["Cellresult"]]
        Elementlist <- SimResults$all$`_SMC`$Etableresult
      }
      if  (ResultInterface != "SMC cell positions")    {
        Nodelist <-    SimResults$all[[ResultInterface]]$Noderesult
        Elementlist <- SimResults$all[[ResultInterface]]$Etableresult
      }
      
      NodeResultNameList <- SimResults$NodeResultNameList
      resultfileindex <- which(NodeResultNameList == ResultFileName)
      
      nodefileDF <- as.data.frame(Nodelist[,,resultfileindex])
      nodefileDFRef <- nodefileDF %>% filter(Loadstep == as.numeric(ResultLoadStepRef), Substep == as.numeric(ResultSubStepRef))
      nodefileDFLoad <- nodefileDF %>% filter(Loadstep == as.numeric(ResultLoadStepLoad), Substep == as.numeric(ResultSubStepLoad))
      
      
      
      AutoColor <- PlotControlVariables$AutoColor
      ncolor <- PlotControlVariables$ncolor
      ColorPalette <- PlotControlVariables$ColorPalette
      DezCount <- PlotControlVariables$DecimalPlaces
      TitleFontSize <- PlotControlVariables$TitleFontSize
      AxisFontSize <- PlotControlVariables$AxisFontSize
      linesmoothing <- PlotControlVariables$linesmoothing
      showlabels <- PlotControlVariables$showlabels
      showlines <- PlotControlVariables$showlines
      showscale <- TRUE
      
      
      
      if (Subtraction == "Load minus Ref") {
        rescolumnRef <- nodefileDFRef[[ResultType]]
        rescolumnLoad <- nodefileDFLoad[[ResultType]]
        rescolumn <- rescolumnLoad - rescolumnRef
      }
      if (Subtraction == "Reference only") {
        rescolumn <- nodefileDFRef[[ResultType]]
      }
      if (Subtraction == "Load only") {
        rescolumn <- nodefileDFLoad[[ResultType]]
      }
      
      Sx <- (nodefileDFLoad$Sx)
      if (ResultType == "Delamination state") {
        rescolumn <- replace(Sx,Sx != 0, 1)
      }
      
      nodefileDFLoad$Pos_X <- round(nodefileDFLoad$Pos_X, digits = 3)
      nodefileDFLoad$Pos_y <- round(nodefileDFLoad$Pos_y, digits = 3)
      ##########################
      #library(reshape2)
      aaa <- cbind(nodefileDFLoad, resulttype = rescolumn)
      xpositions <- sort(unique(aaa$Pos_X))
      ypositons <- sort(unique(aaa$Pos_y))
      Etablezmatrix <- (acast(aaa, Pos_y~Pos_X, value.var = "resulttype",drop = FALSE))
      ##########################
      #print(PlotControlVariables$ncolor)
      
      SMCcolor <- rev(brewer.pal(11, ColorPalette))
      if (AutoColor == TRUE) {
        mincolor_in <- min(rescolumn,na.rm = TRUE)
        maxcolor_in <- max(rescolumn,na.rm = TRUE)
      }
      
      if (AutoColor == FALSE) {
        mincolor_in <- PlotControlVariables$MIN
        maxcolor_in <- PlotControlVariables$MAX
      }
      
      mybreaks <- c(-Inf,seq(mincolor_in, maxcolor_in, length.out = (ncolor + 1)),Inf)
      mycols <- colorRampPalette(brewer.pal(11, ColorPalette))
      mycols_withgray <- c("#808080",rev(mycols(ncolor)),"#808080")
      
      # aaatest<<-ret
      # clusters.plot=ret$clusters.plot
      # EtableNodeCoords=ret$EtableNodeCoords
      # xpositions = ret$xpositions
      # ypositons = ret$ypositons
      # Etablezmatrix = ret$Etablezmatrix
      # mybreaks=ret$mybreaks 
      # mycols=ret$mycols
      # mycols_withgray=ret$mycols_withgray
      # mincolor_in=ret$mincolor_in
      # maxcolor_in=ret$maxcolor_in
      
      if (!exists("SimResults"))
        return(NULL)
      
      if (ResultFileName == "nothing selected" || ResultType == "nothing selected" )
        return(NULL)
      
      #SimResults<-Si
      
      #plotdf <- data.frame(xpositions = xpositions ,ypositons = ypositons)
      
      # xpositions<-sort(unique(aaa$Pos_X))
      # ypositons<-sort(unique(aaa$Pos_y))
      # Etablezmatrix<-(acast(aaa, Pos_y~Pos_X, value.var="resulttype"))
      # clusters.plot <- plot_ly()
      # add_contour <- clusters.plot %>% 
      Celllabel <- "positions_in_mm"
      # PosX=round(target_minx+( (columns_table) * targed_grid), digits = 2)      #(max(column)-column)
      # PosY=round(target_miny+((max(rows_table)-rows_table)*targed_grid), digits = 2)
      
      PosX <- round(xpositions, digits = 2)
      PosY <- round(ypositons, digits = 2)
      
      marginLayout  <- list(
        l = 1,
        r = 1,
        b = 1,
        t = 60,
        pad = 1
      )
      
      if (Celllabel == "positions_in_mm") {
        xaxis_title <- paste("Cellposition X in mm")# cell column number")
        xaxis_ticktext <- as.list(unique(sort(PosX))) #columnsTxt
        xaxis_tickvals <- as.list(unique(sort(PosX)))
        
        yaxis_title <- paste("Cellposition Y in mm")#"row column number")
        yaxis_ticktext <- as.list(sort(unique(sort(PosY))))#columnsTxt
        yaxis_tickvals <- as.list(sort(unique(sort(PosY))))
      }
      if (Celllabel == "row_and_column_numbers") {
        xaxis_title <- paste("Cell colum number")# cell column number")
        xaxis_ticktext <- as.list(as.character(columns)) #columnsTxt
        xaxis_tickvals <- as.list(sort(unique(PosX)))
        
        yaxis_title <- paste("Cell row number")#"row column number")
        yaxis_ticktext <- as.list(rev(as.character(rows)))#columnsTxt
        yaxis_tickvals <- as.list(sort(unique(PosY)))#as.list((yaxis_ticktext))
        
        #list("One", "Three", "Five")
      }
      # yaxis_ticktext[[1]] <- "+"
      # xaxis_ticktext[[1]] <- "+"
      
      xaxis_title <- "mm"
      yaxis_title <- "mm"
      
      reftxt <- paste("Loadstep: ",ResultLoadStepRef,"Substep: ",ResultSubStepRef)
      Loadtxt <- paste("Loadstep: ",ResultLoadStepLoad,"Substep: ",ResultSubStepLoad)
      
      return(list(Loadtxt = Loadtxt
                  ,reftxt = reftxt
                  ,yaxis_title = yaxis_title
                  ,xaxis_title = xaxis_title
                  ,xaxis_ticktext = xaxis_ticktext
                  ,yaxis_ticktext = yaxis_ticktext
                  ,xaxis_tickvals = xaxis_tickvals
                  ,yaxis_tickvals = yaxis_tickvals
                  ,maxcolor_in = maxcolor_in
                  ,mincolor_in = mincolor_in
                  ,marginLayout = marginLayout
                  ,mybreaks = mybreaks
                  ,mycols = mycols
                  ,mycols_withgray = mycols_withgray
                  ,ypositons = ypositons
                  ,xpositions = xpositions
                  ,Etablezmatrix = Etablezmatrix
                  ,Elementlist = Elementlist
                  ,Nodelist = Nodelist
                  ,NodeResultNameList = NodeResultNameList
                  ,resultfileindex = resultfileindex
                  ,ResultInterface = ResultInterface
                  ,ResultType = ResultType
                  ,ResultFileName = ResultFileName
                  ,ResultLoadStepLoad = ResultLoadStepLoad
                  ,ResultSubStepLoad = ResultSubStepLoad
                  ,ResultLoadStepRef = ResultLoadStepRef
                  ,ResultSubStepRef = ResultSubStepRef
                  ,Subtraction = Subtraction
      ))
    }) %>%
      bindEvent(PlotResultVariables$ResultFileName
                ,input$ResultInterface
                ,PlotResultVariables$ResultLoadStepLoad
                ,PlotResultVariables$ResultSubStepLoad
                ,PlotResultVariables$ResultLoadStepRef
                ,PlotResultVariables$ResultSubStepRef
                ,input$ResultType
                ,PlotResultVariables$Subtraction
                ,PlotControlVariables$AutoColor
                ,PlotControlVariables$ncolor
      )
    
    output$graph <- renderPlotly({
      
      PlotResultVariables$triggers$plot
      PlotControlVariables$triggers$plot
      SimResults$triggers$plot
      
      if (is.null(SimResults$InterfaceList) == TRUE) {
        print("no data is loaded")
        return(empty_plot("no data is loaded"))
      }
      
      ret <- ReactivePlotVariables()
      Loadtxt <- ret$Loadtxt
      reftxt  <- ret$reftxt
      yaxis_title <- ret$yaxis_title
      xaxis_title <- ret$xaxis_title
      xaxis_ticktext <- ret$xaxis_ticktext
      yaxis_ticktext <- ret$yaxis_ticktext
      xaxis_tickvals <- ret$xaxis_tickvals
      yaxis_tickvals <- ret$yaxis_tickvals
      maxcolor_in <- ret$maxcolor_in
      mincolor_in  <- ret$mincolor_in
      marginLayout <-  ret$marginLayout
      mybreaks <- ret$mybreaks
      mycols <- ret$mycols
      mycols_withgray  <- ret$mycols_withgray
      ypositons <- ret$ypositons
      xpositions <- ret$xpositions
      Etablezmatrix <- ret$Etablezmatrix
      Elementlist <- ret$Elementlist
      Nodelist <- ret$Nodelist
      NodeResultNameList <- ret$NodeResultNameList
      resultfileindex  <- ret$resultfileindex
      ResultType  <- ret$ResultType
      ResultInterface  <- ret$ResultInterface
      ResultFileName  <- ret$ResultFileName
      ResultLoadStepLoad  <- ret$ResultLoadStepLoad
      ResultSubStepLoad  <- ret$ResultSubStepLoad
      ResultLoadStepRef  <- ret$ResultLoadStepRef
      ResultSubStepRef  <- ret$ResultSubStepRef
      Subtraction  <- ret$Subtraction
      
      
      AutoColor <- PlotControlVariables$AutoColor
      ncolor <- PlotControlVariables$ncolor
      ColorPalette <- PlotControlVariables$ColorPalette
      DezCount <- PlotControlVariables$DecimalPlaces
      TitleFontSize <- PlotControlVariables$TitleFontSize
      AxisFontSize <- PlotControlVariables$AxisFontSize
      linesmoothing <- PlotControlVariables$linesmoothing
      showlabels <- PlotControlVariables$showlabels
      showlines <- PlotControlVariables$showlines
      showscale <- TRUE
      
      if (PlotType == "Node") {
        
        clusters.plot <- plot_ly(#plotdf,
          x = ~xpositions
          ,y = ~ypositons
          ,z = Etablezmatrix
          
          ,colors = mycols_withgray
          ,type = "contour"
          ,contours = list(start = mincolor_in
                           ,end = maxcolor_in
                           ,size = (maxcolor_in - mincolor_in) / ncolor
                           ,coloring = "fill" 
                           ,showlabels = showlabels
                           ,showlines = showlines)
          
          ,line = list(smoothing = linesmoothing
                       , color = "black")
          ,colorbar = list(title = ResultType 
                           ,nticks = ncolor + 1
                           ,tickmode = "array"
                           ,tickvals = mybreaks#seq(mincolor_in,maxcolor_in,length.out=colorcount+1) 
                           ,tickformat = paste(".",DezCount,"f",sep = "")
                           ,tickfont = list(family = "Arial", size = AxisFontSize)
                           ,len = 1.0)
          ,showscale = showscale
          ,connectgaps = FALSE
        ) %>%
          layout(scene = list(aspectration = list(x = 1 ,y = 1))
                 ,margin = marginLayout
                 ,title = list(text = paste("<b>", ResultType, "</b> " ,ResultFileName, " " ,ResultInterface," "
                                            ,"\n","<b>", " Ref: ", "</b> ", reftxt,"<b>", "Load: ", "</b> ", Loadtxt)
                               ,font = list(family = font_google("Roboto Mono") , size = TitleFontSize))  #family = "Arial" ,ont_google("Roboto Mono")
                 ,xaxis = list(title = xaxis_title#paste("cell column number")
                               ,titlefont = list(family = "Arial" ,size = AxisFontSize)
                               ,tickfont = list(family = "Arial" ,size = AxisFontSize)
                               ,showgrid = T
                               ,ticktext = xaxis_ticktext #columnsTxt #list(as.character(rev(rows)))
                               ,tickvals = xaxis_tickvals#as.list((columns))#list(1, 3, 5, 7, 9, 11)#list(as.character(rows))
                               ,tickmode = "array"
                 )
                 ,yaxis = list(title = yaxis_title#paste("cell row number")
                               ,titlefont = list(family = "Arial" ,size = AxisFontSize)
                               ,tickfont = list(family = "Arial" ,size = AxisFontSize)
                               ,showgrid = T
                               ,scaleanchor = "x"
                               #,scaleratio = 1
                               ,ticktext = yaxis_ticktext#rowsTxt #list(as.character(rev(rows)))
                               ,tickvals = yaxis_tickvals#as.list((rows))#list(1, 3, 5, 7, 9, 11)#list(as.character(rows))
                               ,tickmode = "array"
                 )
                 ,legend = list(font = list(family = "Arial", size = AxisFontSize))
          )
      }
      
      
      if (PlotType == "Element") {
        
        #EtableNodeCoords<-cbind(EtableNodeCoords,zcolors)
        
        ElementfileDF <- as.data.frame(Elementlist[,,resultfileindex])
        ElementfileDFRef <- ElementfileDF %>% filter(Loadstep == as.numeric(ResultLoadStepRef), Substep == as.numeric(ResultSubStepRef))
        ElementfileDFLoad <- ElementfileDF %>% filter(Loadstep == as.numeric(ResultLoadStepLoad), Substep == as.numeric(ResultSubStepLoad))
        
        
        if (Subtraction == "Load minus Ref") {
          rescolumnRef <- ElementfileDFRef[[ResultType]]
          rescolumnLoad <- ElementfileDFLoad[[ResultType]]
          rescolumn <- rescolumnLoad - rescolumnRef
        }
        if (Subtraction == "Reference only") {
          rescolumn <- ElementfileDFRef[[ResultType]]
        }
        if (Subtraction == "Load only") {
          rescolumn <- ElementfileDFLoad[[ResultType]]
        }
        
        
        if (ResultType == "Delamination state") {
          Sx <- (ElementfileDFLoad$Sx)
          rescolumn <- replace(Sx, Sx != 0, 1)
        }
        
        zcolor <- cut(rescolumn, breaks = ncolor, include.lowest = T)
        
        EtableNodeCoords <- as.data.frame(cbind(Nodelist <- SimResults$all[[ResultInterface]]$`_CoordsEtable`, resulttype = 0))
        EtableNodeCoords <- as.data.frame(cbind(EtableNodeCoords, zcolor = 0))
        
        elemnumbers <- ElementfileDFLoad$Enum
        
        for (i in elemnumbers) {
          indexetable <- which(elemnumbers == i)
          indexcoords <- which(EtableNodeCoords$ElementI == i)
          EtableNodeCoords$resulttype <- replace(EtableNodeCoords$resulttype,indexcoords,rescolumn[indexetable])
          EtableNodeCoords$zcolor <- replace(EtableNodeCoords$zcolor,indexcoords,zcolor[indexetable])
        }
        
        #pl
        ##############################ggplottest
        polygonElement <- NULL
        for (l in elemnumbers) {
          elementnodes <- dplyr::filter(EtableNodeCoords,ElementI == l)
          
          #which(elemnumbers==l)
          
          polygonElement <- rbind(polygonElement,sort_points(elementnodes,x = "X_Coord",y = "Y_Coord"))
        }
        
        p <- ggplot(polygonElement, aes(x = X_Coord, y = Y_Coord)) +
          geom_polygon(aes(fill = resulttype, group = ElementI)) + 
          coord_fixed( ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") #+
        #theme(plot.title = element_text(size = 10,hjust = 0.5)) #+
        #geom_contour_filled(breaks = mybreaks) #+
        # scale_fill_manual(values = ycols_withgray, drop = FALSE) #+
        # guides(fill = guide_colorsteps(
        #   #direction = "horizontal",
        #   barheight = unit(par("pin")[2]
        #                    #barwidth = unit(par("pin")[1]
        #                    , "in")))
        ggplotly(p)
        
        ##############################
        
        # EtableNodePolygons.plot <- plot_ly(  
        #   
        #   # width = "100%",
        #   # height = "100%"
        # )

        
        Etableoverlay <- FALSE
        if (Etableoverlay == TRUE) {
          
          clusters.plot <- plot_ly()
          
          for (l in elemnumbers) {
            
            elementnodes <- dplyr::filter(EtableNodeCoords,ElementI == l)
            
            #which(elemnumbers==l)
            
            polygonElement <- sort_points(elementnodes,x = "X_Coord",y = "Y_Coord")
            
            #zcolors<-cut(rescolumn, breaks = ncolor, include.lowest = T)
            
            clusters.plot <- clusters.plot %>%
              add_polygons(data = polygonElement, x = ~X_Coord, y = ~Y_Coord
                           #,colors = ~mycols_withgray
                           #,color= ~zcolors
                           ,name = paste0("Element: ",l," " ,ResultType,"= "),
                           line = list(width = 1,color = "black"),
                           fillcolor = mycols_withgray[polygonElement$zcolor[1]],
                           #hoverinfo = l,
                           showlegend = FALSE,
                           inherit = FALSE
              )
            
            # clusters.plot <- clusters.plot %>%
            #   add_text(data = polygonElement, x = mean(polygonElement$X_Coord),y = mean(polygonElement$Y_Coord)
            #            ,text = polygonElement[1,1] #paste(,PlotControlVariables$ColorPalette) #
            #            ,showlegend = FALSE
            #            ,inherit = FALSE
            #            )
          }
        }
      }
      # toWebGL(clusters.plot)  #Warnung in verify_webgl(p) The following traces don't have a WebGL equivalent: 1
      clusters.plot
    })
  })
  
}