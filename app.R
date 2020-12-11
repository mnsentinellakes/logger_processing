#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(devtools)
library(ContDataQC)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(grDevices)
library(xts)
library(readr)
library(rmarkdown)
library(DT)
library(mnsentinellakes)
library(ids)
library(lubridate)
source("functions/dashboard.R")
options(scipen = 999)
# function definitions are located in the dashboard.R file
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(
        #Title
        title = "Data Logger Processing",
        titleWidth = 275,
        enable_rightsidebar = FALSE
    ),
    rightsidebar = uiOutput("rtsidebar"),
    sidebar = dashboardSidebar(
        logprocsidebar("leftsidebarmenu")
    ),
    body = dashboardBody(
        #Defined in the maintable.R file
        source("functions/maintabs.R",local = TRUE)$value
    )
)

# Define server logic 
server <- function(input,output,session) {
    
    #Code for loading baseconfig data, holding all metadata and configuration data
    source("functions/baseconfigload.R",local = TRUE)$value
    
    #Code for configuration setup
    source("functions/qcconfig.R",local = TRUE)$value
    
    #Code for qc values update
    source("functions/qcvalues.R",local = TRUE)$value
    
    #Code for program management
    source("functions/programmanagement.R",local = TRUE)$value
    
    #Code for logger file definitions
    source("functions/logfiledefs.R",local = TRUE)$value
    
    #Code of export options
    source("functions/exportoptions.R",local = TRUE)$value
    
    #Code for saving the configuration file
    source("functions/saveconfigfile.R",local = TRUE)$value
    
    #code for QC processing and data formatting UI
    source("functions/processingUI.R", local = TRUE)$value
    
    #code for QC processing and data formatting
    source("functions/processing.R", local = TRUE)$value
    
    #code for the depths table
    source("functions/depthstable.R",local = TRUE)$value
    
    #code for alerts
    source("functions/alerts.R",local = TRUE)$value
    
    #code for VisQC UI
    source("functions/visqcUI.R",local = TRUE)$value
    
    #Code for VisQC Plot
    source("functions/visqcPlot.R",local = TRUE)$value
    
    #Code for data summary
    source("functions/summary.R",local = TRUE)$value
    
    #Code for data export
    source("functions/exportprocessing.R",local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)

#Future update ideas
# -change qc thresholds according to depth
# -indicate whether a logger model will be processed with multiple units, ie depths
# -allow the user to rearrange the export fields


#Notes

# -check for naming inconsistencies
#   -make sure UI objects are named with "UI" at the end
# -add saved notifications where needed
# -add many notes and documentation


