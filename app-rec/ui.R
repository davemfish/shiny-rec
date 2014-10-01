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
  
  navbarPage("Coastal Vulnerability Results",
             
             tabPanel("Get Started",
                      sidebarLayout(
                        sidebarPanel(
                          #selectInput("InVEST", label="Choose InVEST workspace", choices=getdir(), selected=NULL),
                          h4("Select an InVEST run to visualize"),
                          p("Browse to the directory you defined as your workspace when you ran the InVEST model. 
                            Your workspace contains these 2 subfolders:"),
                          p("     'results-year-mn-dy--hh_mm_ss'"),
                          p("     'tmp'"),
                          textInput("InVEST", "", "Enter Workspace Path"),
                          actionButton("ChooseDir", "Browse"),
                          tags$br(),
                          actionButton("upload", strong('Upload Results')),
                          p(tags$small(em("(this may take a moment)"))),
                          br(),
                          h4("Explore your results"),
                          p("After clicking 'Upload Results', information about the parameters of your InVEST run will appear on the 'About' tab.
                            To visualize a different InVEST run, browse to a different workspace, and click 'Upload' again. 
                            To compare results from two model runs, use the 'Compare Scenarios' tab.")
                          ),
                        mainPanel(
                          selectInput("mapvar2", label="Map Layer", choices=NULL),
                          chartOutput("Rleafmap", 'leaflet'),
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
                          downloadButton("downloadCSV", label = "Download CSV", class = NULL)
                        ),
                        mainPanel(
                          dataTableOutput("printtable"))
                      )
             ),
             tabPanel("Compare Scenarios",
                      sidebarLayout(
                        sidebarPanel(
                          p("Use this tab to compare results of two InVEST runs.
                            For example, you can visualize the differences between a baseline scenario and an additional scenario."),
                          
                          tags$br(),
                          h5("Baseline"),
                          textInput("Baseline", "", "Baseline workspace"),
                          actionButton("ChooseBase", "Browse"),
                          tags$br(),
                          tags$br(),
                          h5("Scenario"),
                          textInput("Scenario", "", "Scenario workspace"),
                          actionButton("ChooseScen", "Browse"),
                          tags$br(),
                          tags$br(),
                          actionButton("Difference", strong("Compare Results")),
                          p(tags$small(em("(this may take a moment)"))),
                          
                          p("After clicking 'Compare Results', values at each coastal segment 
                            of the 'Baseline' workspace are subtracted from corresponding values 
                            in the 'Scenario' workspace.")
                          ),
                        mainPanel(
                          uiOutput("diffnames"),
                          tags$head(tags$script(src="leafletbug_fix.js")),
                          chartOutput("Rleafmap2", 'leaflet'),
                          p(strong("BLUE"), "represents a", strong("less vulnerable"), "coastline under the scenario, compared to the baseline."),
                          p(strong('RED'), "represents a", strong("more vulnerable"), "coastline under the scenario, compared to the baseline."),
                          p("Size of the dot represents the magnitude of change.  Gray dots appear where there is no change.")
                          
                        )
                      )
             ),
             
             tabPanel("About",
                      p("Currently viewing results from this directory:"),
                      h5(em(textOutput("directory"))),
                      p("The table below contains information from the logfile generated by the InVEST run. 
                        All the values for the Coastal Vulnerability Model input parameters are displayed here."),
                      tableOutput("config"),
                      br(),
                      h3("About this application"),
                      p("This web application reads the output workspace for your recent InVEST run, 
                        and displays some of the results. You may also wish to explore and analyze your results 
                        further with GIS software or other data analysis software."),
                      p("This application is built with R and Shiny. The source code is available and
                        you are encouraged to submit bugs at https://github.com/davemfish/shiny-cv")
             )             
))#)
