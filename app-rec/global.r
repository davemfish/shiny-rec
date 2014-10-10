Cut2Num <- function(x){
  ids <- unique(as.numeric(x))
  char.x <- as.character(levels(x))
  num.x <- as.numeric(gsub(unlist(strsplit(char.x, split=",")), pattern='\\(|\\]', replacement=""))
  return(list(brks=unique(num.x), ids=ids))
}

# recTemplate <- "<script>
#                 new L.Control.Zoom({ position: 'topright' }).addTo(map);
#                 </script>"


### a mod of rCharts::renderMap to add on custom javascript to the 
### HTML that shiny ui will render. The js should be leaflet and 
### should be applied as follows:

# recTemplate <- "<script>
#                 new L.Control.Zoom({ position: 'topright' }).addTo(map);
#                 </script>"
# L0$setTemplate(afterScript = recTemplate)

renderMap2 <- function (expr, env = parent.frame(), quoted = FALSE, html_sub = NULL) 
{
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    rChart_ <- func()
    map_style <- sprintf("<style>.leaflet {width: %spx; height: %spx} </style>", 
                         rChart_$params$width, rChart_$params$height)
    map_div = sprintf("<div id=\"%s\" class=\"rChart leaflet\"></div>", 
                      rChart_$params$dom)
    rChart_html = rChart_$html()
    if (length(html_sub) > 0) {
      for (i in 1:length(html_sub)) {
        rChart_html = gsub(names(html_sub)[i], as.character(html_sub[i]), 
                           rChart_html)
      }
    }
    ### edited this line to append rcharts afterScript slot into output HTML
    HTML(paste(c(map_style, map_div, rChart_html, rChart_$templates$afterScript), collapse = "\n"))
  }
}