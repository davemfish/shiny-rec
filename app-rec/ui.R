library(shiny)
library(rCharts)

# 
# 
# textInputRow <- function (inputId, label, value = "", bgcol="red") {
#   div(style=paste("display:inline-block; background-color:", bgcol, sep=""),
#       tags$label(label, `for` = inputId), 
#       tags$input(id = inputId, type = "text", value = value,class="input-mini"))
# }

# Define UI 
shinyUI(#fluidPage(
  
  navbarPage("Recreation Results",
             
             tabPanel("Get Started",
                      sidebarLayout(
                        sidebarPanel(
                          #selectInput("InVEST", label="Choose InVEST workspace", choices=getdir(), selected=NULL),
                          h4("Select an InVEST run to visualize"),
                          p("Browse to the logfile contained in the workspace you defined when you ran the InVEST model. 
                            The logfile will have a name such as:"),
                          p("recreation_client-log-2014-03-31--10_36_58.txt"),
                          fileInput("log", ""),
#                           textInput("InVEST", "", "Enter Workspace Path"),
#                           actionButton("ChooseDir", "Browse"),
#                           tags$br(),
                           actionButton("upload", strong('Display Results')),
                           p(tags$small(em("(this may take a moment)"))),
                           br(),
                          h4("Explore your results"),
                          p("After clicking 'Display Results', information about the parameters of your InVEST run will appear on the 'About' tab.
                            To visualize a different InVEST run, browse to a different logfile, and click 'Display' again.")
                          ),
                        mainPanel(
                          #HTML('<style type="text/css"> .span8 .selectize-control { float: right; } </style>'),
                          radioButtons("mapvar2", label="Map Layer:", choices=NA, inline=T),
                          chartOutput("Rleafmap", 'leaflet'),
                          #HTML("<script> new L.Control.Zoom({ position: 'topright' }).addTo(map); </script>"),
                          #tags$script(type='text/javascript', "new L.Control.Zoom({ position: 'topright' }).addTo(map);"),
                          h5(""),
                          br(),
                          plotOutput("hist2")
                        )
                          )
                      ),
             tabPanel("Tables", 
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("tablenames"),
                          br(),
                          downloadButton("downloadCSV", label = "Download CSV", class = NULL),
                          br(),
                          br(),
                          p(tags$small("These data can also be accessed in the attribute table of the 'grid.shp' shapefile in the results folder of your workspace."))
                        ),
                        mainPanel(
                          dataTableOutput("printtable"))
                      )
             ),
#              tabPanel("Compare Scenarios",
#                       sidebarLayout(
#                         sidebarPanel(
#                           p("Use this tab to compare results of two InVEST runs.
#                             For example, you can visualize the differences between a baseline scenario and an additional scenario."),
#                           
#                           tags$br(),
#                           h5("Baseline"),
#                           textInput("Baseline", "", "Baseline workspace"),
#                           actionButton("ChooseBase", "Browse"),
#                           tags$br(),
#                           tags$br(),
#                           h5("Scenario"),
#                           textInput("Scenario", "", "Scenario workspace"),
#                           actionButton("ChooseScen", "Browse"),
#                           tags$br(),
#                           tags$br(),
#                           actionButton("Difference", strong("Compare Results")),
#                           p(tags$small(em("(this may take a moment)"))),
#                           
#                           p("After clicking 'Compare Results', values at each coastal segment 
#                             of the 'Baseline' workspace are subtracted from corresponding values 
#                             in the 'Scenario' workspace.")
#                           ),
#                         mainPanel(
#                           uiOutput("diffnames"),
#                           tags$head(tags$script(src="leafletbug_fix.js")),
#                           chartOutput("Rleafmap2", 'leaflet'),
#                           p(strong("BLUE"), "represents a", strong("less vulnerable"), "coastline under the scenario, compared to the baseline."),
#                           p(strong('RED'), "represents a", strong("more vulnerable"), "coastline under the scenario, compared to the baseline."),
#                           p("Size of the dot represents the magnitude of change.  Gray dots appear where there is no change.")
#                           
#                         )
#                       )
#              ),
             
             tabPanel("About",
                      p("The table below contains information from the logfile generated by the InVEST run. 
                        All the values for the Recreation Model input parameters are displayed here."),
                      tableOutput("config"),
                      br(),
                      h3("About this application"),
                      p("This web application reads the logfile for your recent InVEST run and displays some of the results. 
                        The data displayed are not read from your local computer, 
                        rather you are viewing a duplicate version of your results stored on the Recreation model server. 
                        You may also wish to explore and analyze your results 
                        further with GIS software or other data analysis software."),
                      p("This application is built with R and Shiny. The source code is available and
                        you are encouraged to submit bugs at https://github.com/davemfish/shiny-rec")
             )             
))#)
