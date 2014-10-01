library(shiny)
library(ggplot2)
library(reshape2)
library(rCharts)
library(rgdal)
library(raster)
library(RColorBrewer)
library(hwriter)




### ggplot theme ####
th.bar <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=11),
                axis.text.x=element_text(size=11),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14),
                strip.text=element_text(size=11), 
                strip.background=element_blank(), 
                panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="none")

th.hist <- theme(panel.background = element_rect(fill="white"), 
                 axis.text.y=element_text(size=12),
                 axis.text.x=element_text(size=12),
                 #axis.title.x=element_blank(),
                 #axis.title.y=element_blank(),
                 #strip.text=element_blank(), 
                 strip.background=element_blank(), 
                 panel.border = element_rect(color="black", fill=NA), 
                 #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                 panel.grid.major.y=element_blank(),
                 panel.grid.minor.y=element_blank(),
                 panel.grid.minor.x=element_blank(),
                 panel.grid.major.x=element_blank(),
                 #legend.text=element_blank(),
                 legend.position="none")


print("start function")


## This function loads and processes the coastal_exposure.csv
## Its called inside LoadONE() OR LoadTWO() - comparison
LoadSpace <- function(inputX){
  ws <- inputX
  unzip(Sys.glob(file.path(ws, "results*.zip")), exdir=ws, overwrite=T)
  grid <- readOGR(dsn=ws, layer="grid")
  #aoi <- raster(file.path(ws, "intermediate/00_preprocessing/00_PRE_aoi.tif"))
  #points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
  if (grepl("\\+proj=longlat\\s+\\+datum=WGS84", projection(grid))){
    print("Checking AOI CRS...OK")
  } else {
    print("....transforming to longlat WGS84")
    grid.wgs84 <- spTransform(grid, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  #   ce <- cbind(points.wgs84, ce)
  #   names(ce)[1:2] <- c("lon", "lat")
  #   print("loaded csv")
  return(grid.wgs84)
}

L0 <- Leaflet$new()
L0$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
L0$setView(c(0, 0), 1)  
L0$set(width = 550, height = 450) 

###### Server Function ##############
shinyServer(function(input, output, session) {
  
  ## Browse to directory
  observe({ 
    if (input$ChooseDir == 0)
      return(NULL)
    
    dirname <- choose.dir()
    #str(input$ChooseDir)
    isolate({
      updateTextInput(session, "InVEST", "InVEST Workspace", value=dirname)
    })
  })
  
  ## def function to upload results from dir
  loadONE <- reactive({ 
    if (input$upload == 0)
      return(NULL)
    
    isolate({
      grid <- LoadSpace(input$InVEST)
      return(grid)
    })
  })
  
  ## COMP: Browse to baseline directory 
  observe({ 
    if (input$ChooseBase == 0)
      return(NULL)
    
    dirname <- choose.dir()
    isolate({
      updateTextInput(session, "Baseline", "", value=dirname)
    })
  })
  
  ## COMP: Browse to scenario directory 
  observe({ 
    if (input$ChooseScen == 0)
      return(NULL)
    
    dirname <- choose.dir()
    isolate({
      updateTextInput(session, "Scenario", "", value=dirname)
    })
  })
  
  ## COMP: def function to upload both sets of results
  loadTWO <- reactive({
    if (input$Difference == 0)
      return(NULL)
    isolate({
      ce.base <- LoadSpace(input$Baseline)
      ce.scen <- LoadSpace(input$Scenario)
      return(list(ce.base, ce.scen))
    })
  })
  
  ## Read InVEST logfile.txt
  loadLOG <- reactive({
    if (input$upload == 0)
      return(NULL)
    
    #isolate({
    #input$upload
    ws <- input$InVEST
    logfile <- readLines(con=file.path(ws, list.files(path=ws, pattern=glob2rx("recreation_client-log*.txt"))), n=-1)
    blanks <- which(logfile=="")
    log <- logfile[1:(min(blanks) - 1)]
    print("loaded log")
    print(log[6])
    return(log)
    #})
    
  })
  
  ## Render the logfile on the About page
  output$config <- renderTable({
    if (input$upload == 0)
      return(NULL)
    isolate({ matrix(loadLOG()) })
  })
  output$directory <- renderText({
    if (input$upload == 0)
      return(NULL)
    isolate({
      tail(unlist(strsplit(tail(loadLOG(), 1), split=" ")), 1)
    })
  })
  
  ## PLOT: set map layer input variable
  ## requires uploading results with loadONE()
  observe({
    if (input$upload == 0)
      return(NULL)
    isolate({
      print("updating select")
      grid <- loadONE()
      
      updateSelectInput(session, "mapvar2",
                        label = "Map Layer",
                        choices = names(grid@data)[2:5],
                        selected = "usdyav"
      )
    })
  })
  
  
  
  ## PLoT: def function to apply a color pallette to map variable
  ## requires uploading with loadONE()
  getCol <- reactive({
    if (input$upload == 0)
      return(NULL)
    if (is.null(input$mapvar2))
      return(NULL)
    if (input$mapvar2 == "")
      return(NULL)
    
    isolate({
      grid <- loadONE()
      dat <- grid@data
      #print(class(ce))
      #print(input$mapvar2)
      cols <- brewer.pal(8, "BuPu")[as.numeric(cut(dat[[input$mapvar2]], breaks=8))]
      #print(head(cols))
      return(cols)
    })
  })
  
  
  ## Initialize first Leaflet Map
  # L1 <- Leaflet$new()
  # #L1$addAssets(jshead = "https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
  # L1$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
  # L1$set(width = 550, height = 450) 
  
  ## PLOT: def function to add points and set view of leaflet
  ## calls to loadONE(), getCol()
  plotMap <- reactive({
    
    if (input$upload == 0)
      return(NULL)
    if (input$mapvar2 == "")
      return(NULL)
    if (is.null(input$mapvar2))
      return(NULL)
    print("plotMap past NULLs")
    grid <- loadONE()
    grid@data <- grid@data[,1:5]
    grid@data$col <- getCol()

    if (!file.exists(file.path(".", "tmpdir"))){
      tmppath <- dir.create(file.path(".", "tmpdir"))
    } else {
      tmppath <- file.path(".", "tmpdir")
    }
    jsonfile <- tempfile("grid", tmpdir=tmppath)
    print("WRITING geoJSON")
    writeOGR(grid, jsonfile, layer="", driver="GeoJSON", overwrite_layer=T)
    print("READING geoJSON")
    grid.list <- RJSONIO::fromJSON("./tmpdir/gridjson.geojson")
    grid.list[[3]] <- lapply(grid.list[[3]], function(x){
      mat <- as.matrix(unlist(x))
      mat <- as.matrix(mat[(grep("geometry*", rownames(mat))*-1),])
      x$popup <- hwrite(mat)
      return(x)
    })
    L0$setView(c(mean(c(bbox(grid)[2,1], bbox(grid)[2,2])),mean(c(bbox(grid)[1,1], bbox(grid)[1,2])), 8))
    L0$geoJson(grid.list, 
           onEachFeature = "#! function(feature, layer){
            layer.bindPopup(feature.popup)
           } !#",
           style="#! function style(feature){
            return {
              fill:true,
              fillColor: feature.properties.col,
              fillOpacity:0.7,
              color:'white',
              weight:1
            };
           } !#"
    )
    return(L0)
    })
  
  ## PLOT: render 1st Leaflet
  output$Rleafmap <- renderMap({
    if (input$upload == 0)
      return(L0)
    if (is.null(input$mapvar2))
      return(L0)
    if (input$mapvar2 == "")
      return(L0)
    plotMap()
  })
  
  
  ## PLOT: render array of histograms

  
  ## COMP: 
  ## Initialize 2nd leaflet map
  L2 <- Leaflet$new()
  #L1$addAssets(jshead = "https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
  L2$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
  L2$set(width = 550, height = 450)  
  
  ## COMP:
  ## def function that differences values in 2 scenarios
  ## requires upload with loadTWO()
  Difference <- reactive({
    if (input$Difference == 0)
      return(NULL)
    if (is.null(input$fieldnames))
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
      diff <- df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames]
      diff <- cbind(df.base[,c("lat", "lon")], diff)
      names(diff) <- c("lat", "lon", "delta")
      diff$baseline <- df.base[ ,input$fieldnames]
      diff$scenario <- df.scen[ ,input$fieldnames]
      print("diff summ")
      print(summary(diff))
    })
    return(diff)
  })
  
  ## COMP:
  ## def function for assigning color to map variable, which is the differenced value
  ## calls Difference(), Breaks() 
  getCol2 <- reactive({
    if (input$Difference == 0)
      return(NULL)
    if (is.null(input$fieldnames))
      return(NULL)
    #print(input$Symbolize)
    #print(input$Breaks3)
    
    #isolate({
    isolate({
      diff <- Difference()
      colbrks <- as.numeric(cut(diff$delta, breaks=c(-10, -0.0001, 0.0001, 10), labels=F))
    })
    print("Diff info")
    
    print(summary(diff$delta))
    cols <- c(rgb(0,0,1), rgb(1,1,1), rgb(1,0,0))[colbrks]
    print("cols info")
    print(head(colbrks))
    print(summary(colbrks))
    print(head(cols))
    print(length(cols))
    return(cols)
    #})
  })
  
  
  
  
  ## COMP:
  ## def function to add points to comparison map
  ## calss to getCol2(), 
  plotMap2 <- reactive({
    if (input$Difference == 0)
      return(NULL)
    if (is.null(input$fieldnames))
      return(NULL)
    
    df.diff <- Difference()
    df.diff$col <- getCol2()
    #print("what's the class")
    #print(class(df.diff$delta))
    df.diff$circ <- sapply(df.diff$delta, FUN=function(x){((sqrt(abs(x)/pi))+1.5)^2.5})
    tmp.diff <- apply(df.diff, 1, as.list)
    tmp.diff <- lapply(tmp.diff, function(x){
      mat <- as.matrix(unlist(x))
      mat <- as.matrix(mat[!(rownames(mat) %in% c("x", "y", "array.row", "array.col", "col", "circ")),])
      x$popup <- hwrite(mat)
      return(x)
    })
    
    L2$setView(c(mean(df.diff$lat), mean(df.diff$lon)), 9)
    L2$geoJson(toGeoJSON(tmp.diff, lat='lat', lon='lon'), 
               onEachFeature = '#! function(feature, layer){
               layer.bindPopup(feature.properties.popup)
  } !#',
               pointToLayer =  "#! function(feature, latlng){
               return L.circleMarker(latlng, {
               radius: feature.properties.circ,
               fillColor: feature.properties.col || 'white',    
               color: '#000',
               weight: 1,
               fillOpacity: 0.65
               })
               } !#")
    return(L2)
    })
  
  #radius: sqrt(abs(feature.properties.delta)/3.14159)+1
  
  ## COMP:
  ## Select boxes with variables common to both scenarios. Used to build a table.
  ## requires uploading results with loadTWO()
  output$diffnames <- renderUI({
    if (input$Difference == 0)
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
    })
    selectInput("fieldnames", 
                label="Select values to compare", 
                choices=intersect(names(df.base)[-6:-1], names(df.scen)),
                selected = "coastal_exposure")
  })
  
  #   output$difftable <- renderDataTable({
  #     if (input$diffcalc == 0)
  #       return(NULL)
  #     isolate({
  #       df.base <- loadTWO()[[1]]
  #       df.scen <- loadTWO()[[2]]
  #       df.diff <- data.frame(df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames])
  #       names(df.diff) <- input$fieldnames
  #       print(class(df.diff))
  #       print(names(df.diff))
  #       df.diff <- cbind(df.base[,c("lat", "lon")], df.diff)
  #     })
  #     return(df.diff)
  #   })
  
  output$Rleafmap2 <- renderMap({
    if (input$Difference == 0){
      L01 <- Leaflet$new()
      L01$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
      L01$setView(c(0, 0), 1)  
      L01$set(width = 550, height = 450) 
      return(L01)
    }
    plotMap2()
  })
  
  
  ### TABLE tab:
  output$tablenames <- renderUI({
    if (input$upload == 0)
      return(NULL)
    isolate({
      df <- loadONE()
    })
    checkboxGroupInput("tablenames", 
                       label="Select columns for the table", 
                       choices=names(df),
                       selected = c("lon", "lat", "coastal_exposure")
    )
  })
  
  FormatTable <- reactive({ 
    if (is.null(input$tablenames))
      return(NULL)
    isolate({
      df <- loadONE()
      print(names(df))
      print(input$tablenames)
      df <- format(df[,input$tablenames], nsmall=3, digits=3)
    })
    #print()
    return(df)
  })
  
  output$printtable <- renderDataTable({
    FormatTable()
  })
  
  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(FormatTable(), file)}
  )
  
  })