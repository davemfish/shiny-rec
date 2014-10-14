library(shiny)
library(ggplot2)
library(reshape2)
library(rCharts)
library(rgdal)
library(raster)
library(RColorBrewer)
library(hwriter)
library(foreign)
library(RJSONIO)




# ### ggplot theme ####
# th.bar <- theme(panel.background = element_rect(fill="white"), 
#                 axis.text.y=element_text(size=11),
#                 axis.text.x=element_text(size=11),
#                 axis.title.x=element_text(size=14),
#                 axis.title.y=element_text(size=14),
#                 strip.text=element_text(size=11), 
#                 strip.background=element_blank(), 
#                 panel.border = element_rect(color="black", fill=NA), 
#                 #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
#                 panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
#                 panel.grid.minor.y=element_blank(),
#                 panel.grid.minor.x=element_blank(),
#                 panel.grid.major.x=element_blank(),
#                 #legend.text=element_blank(),
#                 legend.position="none")
# 
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




L0 <- Leaflet$new()
L0$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
L0$setView(c(0, 0), 1)  
L0$set(width = 550, height = 450)
L0$mapOpts(zoomControl=FALSE) 
recTemplate <- "<script>
                new L.Control.Zoom({ position: 'topright' }).addTo(map);
                </script>"
L0$setTemplate(afterScript = recTemplate)

## COMP: 
## Initialize 2nd leaflet map
L2 <- Leaflet$new()
L2$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
L2$setView(c(0, 0), 1)  
L2$set(width = 550, height = 450)
L2$mapOpts(zoomControl=FALSE) 
L2$setTemplate(afterScript = recTemplate)

###### Server Function ##############
shinyServer(function(input, output, session) {
  
  ## Read InVEST logfile.txt
  loadLOG <- reactive({
    if (input$upload == 0)
      return(NULL)
    
    init <- getLog(input$log$datapath)
    print(init[["sessid"]])
    return(init)  
  })
  
  loadLOG2 <- reactive({
    if (input$Difference == 0)
      return(NULL)
      
    base <- getLog(input$base$datapath)
    scen <- getLog(input$scen$datapath)
    print(base[["sessid"]])
    return(list(base=base, scen=scen))
  })
  
  LoadSpace <- reactive({
    init <- getSpace(loadLOG()[["sessid"]])
    return(init)
#     sessid <- loadLOG()[[1]]
#     #sessid <- "jvv94vs9fl7pflqmkd91aksm61"
#     ws <- file.path("http://ncp-skookum.stanford.edu/~woodsp", sessid)
#     #unzip(Sys.glob(file.path(ws, "results*.zip")), exdir=ws, overwrite=T)
#     atts <- read.csv(file.path(ws, "grid.csv"))
#     geom <- fromJSON(file.path(ws, "grid.geojson"))
#     aoijson <- fromJSON(file.path(ws, "aoi.geojson"))
#     aoibbox <- aoijson[[2]][[1]]$bbox
#     center <- c(mean(c(aoibbox[2], aoibbox[4])), mean(c(aoibbox[1], aoibbox[3])))    
#     zoom <- 8
#     view <- list(center=center, zoom=zoom)
#     return(list(atts=atts, geom=geom, view=view))
  })

  LoadSpace2 <- reactive({
    print(loadLOG2()[["base"]][["sessid"]])
    base <- getSpace(loadLOG2()[["base"]][["sessid"]])
    scen <- getSpace(loadLOG2()[["scen"]][["sessid"]], two=TRUE)
    return(list(basedata=base[["atts"]], scendata=scen[["atts"]], geom=base[["geom"]], view=base[["view"]]))
  })
  
## COMP:
## Select boxes with variables common to both scenarios. Used to build a table.
## requires uploading results with loadTWO()
output$diffnames <- renderUI({
  if (input$Difference == 0)
    return(NULL)
  isolate({
    df.base <- LoadSpace2()[["basedata"]]
    df.scen <- LoadSpace2()[["scendata"]]
    diffnames <- intersect(names(df.base), names(df.scen))
    diffnames <- diffnames[!(diffnames %in% c("cellID", "cellArea"))]
  })
  selectInput("fieldnames", 
              label="Select values to compare", 
              choices=diffnames,
              selected = "usdyav_est")
})

  ## COMP:
  ## def function that differences values in 2 scenarios
  ## requires upload with loadTWO()
  Difference <- reactive({
    if (input$Difference == 0)
      return(NULL)
    if (is.null(input$fieldnames))
      return(NULL)
    isolate({
      df.base <- LoadSpace2()[["basedata"]]
      df.scen <- LoadSpace2()[["scendata"]]
      
      baseline <- df.base[ ,input$fieldnames]
      scenario <- df.scen[ ,input$fieldnames]
      diff <- scenario - baseline
      
#      diff <- df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames]

      diff <- data.frame(cbind(df.base[ ,c("cellID")], diff))

      names(diff) <- c("cellID", "delta")
#       diff$baseline <- df.base[ ,input$fieldnames]
#       diff$scenario <- df.scen[ ,input$fieldnames]
      diff$baseline <- baseline
      diff$scenario <- scenario

    })
    return(diff)
  })

  
  
  ## Render the logfile on the About page
  output$config <- renderTable({
    if (input$upload == 0)
      return(NULL)
    print(loadLOG()[[2]])
    isolate({ matrix(loadLOG()[[2]]) })
  })
#   output$directory <- renderText({
#     if (input$upload == 0)
#       return(NULL)
#     isolate({
#       tail(unlist(strsplit(tail(loadLOG(), 1), split=" ")), 1)
#     })
#   })
  
  ## PLOT: set map layer input variable
  ## requires uploading results with loadONE()
  observe({
    if (input$upload == 0)
      return(NULL)
    isolate({
      print("updating select")
      atts <- LoadSpace()[["atts"]]
      
      updateRadioButtons(session, "mapvar2",
                        label = "Map Layer:",
                        choices = names(atts)[!(names(atts) %in% c("cellID", "cellArea"))],
                        selected = "usdyav",
                        inline=T
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
      atts <- LoadSpace()[["atts"]]
      dat <- atts[[input$mapvar2]]
      #print(class(ce))
      #print(input$mapvar2)
      if (input$mapvar2 %in% c("usdyav", "usdyav_pr", "usdyav_est")){
        ramp <- "BuPu"
      } else {
        ramp <- "Oranges"
      }
      if (input$mapvar2 %in% c("usdyav", "usdyav_est")){
        ## log transform data and get breaks
        brks <- cut(log(dat+1), breaks=6)
        cols <- as.list(brewer.pal(6, ramp)[as.numeric(brks)])
        ## convert brks factor to numeric
        brks.list <- Cut2Num(brks)
        num.brks <- brks.list[["brks"]]
        ## back-transform numeric breaks to real values
        legbrks <- round(exp(num.brks)-1, digits=3)
      } else {
        brks <- cut(dat, breaks=6)
        cols <- as.list(brewer.pal(6, ramp)[as.numeric(brks)])
        brks.list <- Cut2Num(brks)
        num.brks <- brks.list[["brks"]]
        legbrks <- round(num.brks, digits=3)
      }
      
      legbrks[1] <- 0
      ids <- brks.list[["ids"]]
      ids <- ids[order(ids)]
      ids <- c(1, ids+1)
      legbrks <- legbrks[ids]
      
      leglabs <- list()
      for (i in 1:length(legbrks)){
        if (i == 1) { 
          leglabs[[i]] <- legbrks[i] 
        } else {
          leglabs[[i]] <- paste(legbrks[i-1], "-", legbrks[i])
        }
      }
      
      legcols <- c("#606060", brewer.pal(6, ramp))[ids]

      return(list(cols=cols, legcols=legcols, leglabs=leglabs))
    })
  })
  
  
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
    #atts <- LoadSpace()[["atts"]]
    #atts <- atts[,1:5]
    style <- getCol()
    cols <- style[["cols"]]
    
    isolate({
      print("reloading JSON?")
      grid <- LoadSpace()[["geom"]]
      view <- LoadSpace()[["view"]]
    })
#     grid[[2]] <- lapply(grid[[2]], function(x){
#       mat <- as.matrix(unlist(x))
#       mat <- as.matrix(mat[(grep("geometry*", rownames(mat))*-1),])
#       x$popup <- hwrite(mat)
#       x$
#       return(x)
#     })
    for (i in 1:length(grid[[2]])){
      x <- grid[[2]][[i]]
      y <- cols[[i]]
      mat <- as.matrix(unlist(x))
      #   mat <- as.matrix(mat[(grep("bbox*", rownames(mat))*-1),])
      mat <- as.matrix(mat[(grep("geometry*", rownames(mat))*-1),])
      mat <- as.matrix(mat[!(rownames(mat) %in% c("properties.cellArea", "type")),])
      rownames(mat) <- sub(pattern="properties.", replacement="", rownames(mat))
      x$popup <- hwrite(mat)
      pat <- paste("<td>", input$mapvar2, "</td>", sep="")
      
      x$popup <- sub(pattern=pat, replacement=paste("<td><b>", input$mapvar2, "</b></td>", sep=""), x$popup)
      if (x$properties[input$mapvar2] == 0) {
        x$col <- "#606060"
      } else {
        x$col <- y
      }
      grid[[2]][[i]] <- x
    }

    L0$geoJson(grid, 
           onEachFeature = "#! function(feature, layer){
            layer.bindPopup(feature.popup)
            } !#",
           style="#! function style(feature){
            return {
              fill:true,
              fillColor: feature.col,
              fillOpacity:0.7,
              color:'white',
              weight:1
            };
           } !#"
    )
    
    legcols <- style[["legcols"]]
    leglabs <- style[["leglabs"]]
    L0$legend(position="topleft", colors=legcols, labels=leglabs)
    L0$setView(view[["center"]], view[["zoom"]])
    #L0$setTemplate(afterScript = recTemplate)
    return(L0)
    })
  
  ## PLOT: render 1st Leaflet
  output$Rleafmap <- renderMap2({
    if (input$upload == 0)
      return(L0)
    if (is.null(input$mapvar2))
      return(L0)
    if (input$mapvar2 == "")
      return(L0)
    plotMap()
  })
  
  
  ## PLOT: render array of histograms
#   output$hist <- renderPlot({
#     isolate({
#       dat <- LoadSpace()[["atts"]]
#       if (input$mapvar2 %in% c("usdyav", "usdyav_pr")){
#         ramp <- "BuPu"
#       } else {
#         ramp <- "Oranges"
#       }
#       if (input$mapvar2 == "usdyav"){
#         brks <- cut(log(dat+1), breaks=8)
#         cols <- as.list(brewer.pal(8, ramp)[as.numeric(brks)])
#       } else {
#         brks <- cut(dat, breaks=8)
#         cols <- as.list(brewer.pal(8, ramp)[as.numeric(brks)])
#       }
#     })
#     levs <- levels(brks)
#     ring <- strsplit(levs, split=",")[[1]][1]
#     gghist <- ggplot(dat, aes(x=input$mapvar2)) +
#       geom_bar(stat="bin", binwidth=0.01) +
#       scale_fill_brewer(palette=ramp) +
#       geom_vline(data=data.frame(brk), xintercept=brk, linetype="dashed") +
#       scale_x_continuous(breaks=brk) +
#       #xlim(min(df), max(df)) +
#       th.hist  
#   })
  
  

  
  ## COMP:
  ## def function for assigning color to map variable, which is the differenced value
  ## calls Difference(), Breaks() 
  getCol2 <- reactive({
    if (input$Difference == 0)
      return(NULL)
    if (is.null(input$fieldnames))
      return(NULL)
    
    isolate({
      diff <- Difference()
      diff$delta[is.infinite(diff$delta)] <- NA
      print(summary(diff$delta))
      brks <- as.numeric(cut(diff$delta, breaks=c(min(diff$delta), 0 - sd(diff$delta), -0.0001, 0.0001, 0 + sd(diff$delta), max(diff$delta)), labels=F))
    })
    print("Diff info")
    cols <- as.list(brewer.pal(5, "RdBu")[as.numeric(brks)])
    #cols <- c(rgb(0,0,1), rgb(1,1,1), rgb(1,0,0))[colbrks]
    brks.list <- Cut2Num(brks)
    num.brks <- brks.list[["brks"]]
    legbrks <- round(num.brks, digits=3)
  
    #legbrks[1] <- 0
    ids <- brks.list[["ids"]]
    ids <- ids[order(ids)]
    ids <- c(1, ids+1)
    legbrks <- legbrks[ids]
    
    leglabs <- list()
    for (i in 1:length(legbrks)){
      if (i == 1) { 
        leglabs[[i]] <- legbrks[i] 
      } else {
        leglabs[[i]] <- paste(legbrks[i-1], "-", legbrks[i])
      }
    }
    
    legcols <- c(brewer.pal(5, "RdBu"))[ids]
    
    
    return(list(cols=cols, legcols=legcols, leglabs=leglabs))
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
    #print(names(df.diff))
    #df.diff$col <- getCol2()
    grid <- LoadSpace2()[["geom"]]
    view <- LoadSpace2()[["view"]]
    #print("what's the class")
    #print(class(df.diff$delta))
    df.diff$delta[is.infinite(df.diff$delta)] <- NA
    #df.diff$circ <- sapply(df.diff$delta, FUN=function(x){((sqrt(abs(x)/pi))+1.5)^2.5})
    print(class(df.diff))
    print(df.diff[1,"delta"])
#     tmp.diff <- apply(df.diff, 1, as.list)
#     tmp.diff <- lapply(tmp.diff, function(x){
#       mat <- as.matrix(unlist(x))
#       mat <- as.matrix(mat[!(rownames(mat) %in% c("col", "circ")),])
#       x$popup <- hwrite(mat)
#       return(x)
#     })
    style <- getCol2()
    cols <- style[["cols"]]
    for (i in 1:length(grid[[2]])){
      x <- grid[[2]][[i]]
      x$delta <- df.diff[i,"delta"]
      y <- cols[[i]]
      mat <- as.matrix(unlist(x))
      #   mat <- as.matrix(mat[(grep("bbox*", rownames(mat))*-1),])
      mat <- as.matrix(mat[(grep("geometry*", rownames(mat))*-1),])
      mat <- as.matrix(mat[!(rownames(mat) %in% c("properties.cellArea", "type")),])
      rownames(mat) <- sub(pattern="properties.", replacement="", rownames(mat))
      x$popup <- hwrite(mat)
      pat <- paste("<td>", input$mapvar2, "</td>", sep="")
      
      x$popup <- sub(pattern=pat, replacement=paste("<td><b>", input$mapvar2, "</b></td>", sep=""), x$popup)
#       if (x$properties[input$mapvar2] == 0) {
#         x$col <- "#606060"
#       } else {
#         x$col <- y
#       }
      x$col <- y
      grid[[2]][[i]] <- x
    }
    
    L2$geoJson(grid, 
           onEachFeature = "#! function(feature, layer){
            layer.bindPopup(feature.popup)
            } !#",
           style="#! function style(feature){
            return {
              fill:true,
              fillColor: feature.col,
              fillOpacity:0.7,
              color:'white',
              weight:1
            };
           } !#"
    )
    
    legcols <- style[["legcols"]]
    leglabs <- style[["leglabs"]]
    L2$legend(position="bottomleft", colors=legcols, labels=leglabs)
    L2$setView(view[["center"]], view[["zoom"]])
    return(L2)
    })
  
  #radius: sqrt(abs(feature.properties.delta)/3.14159)+1
  
  
  
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
  
  output$Rleafmap2 <- renderMap2({
    print(str(L2))
    if (input$Difference == 0) 
      return(L2)
    if (is.null(input$fieldnames))
      return(L2)
    if (input$fieldnames == "")
      return(L2)
    plotMap2()
  })
  
  
  ### TABLE tab:
  output$tablenames <- renderUI({
    if (input$upload == 0)
      return(NULL)
    isolate({
      df <- LoadSpace()[["atts"]]
    })
    checkboxGroupInput("tablenames", 
                       label="Select columns for the table", 
                       choices=names(df),
                       selected = c("cellID", "usdyav")
    )
  })
  
  FormatTable <- reactive({ 
    if (is.null(input$tablenames))
      return(NULL)
    isolate({
      df <- LoadSpace()[["atts"]]
      #print(names(df))
      #print(input$tablenames)
      df <- format(df[,input$tablenames], nsmall=3, digits=3)
    })
    #print()
    return(df)
  })
  
  output$printtable <- renderDataTable({
    FormatTable()},
    options=list(pageLength = 10)
  )
  
  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(FormatTable(), file, row.names=F)}
  )
  
  })