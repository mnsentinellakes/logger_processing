#Metadata-----------------------------------------------------------------------
#Name: Logger Processing and QC App
#Version: 1.0
#Purpose: Process and QC Aquatic Loggers
#Developed by: Tim Martin
#Contact: tim.martin@state.mn.us

rm(list = ls())


library(shiny)
library(shinydashboard)
library(devtools)
library(ContDataQC)
library(shinyjs)
library(shinyWidgets)
library(shinyvalidate)
library(dplyr)
library(ggplot2)
library(plotly)
library(grDevices)
library(xts)
library(readr)
library(rmarkdown)
library(DT)
library(ids)
library(lubridate)
library(zip)
library(stringi)

source("functions/dashboard.R")
options(scipen = 999,shiny.fullstacktrace = TRUE)
baseconfigversion = as.character("1.0")
# function definitions are located in the dashboard.R file
ui <- dashboardPage(
    header = dashboardHeader(
        #Title
        title = "Data Logger Processing",
        titleWidth = 275
    ),
    sidebar = dashboardSidebar(
        logprocsidebar("leftsidebarmenu")
    ),
    body = dashboardBody(
      #External Javascript
      tags$head(
        #Get Screen Dimensions
        tags$script(src = "screen_dim.txt")
      ),
      #Code for maintab
      source("functions/maintabs.R",local = TRUE)$value  
    )
)

# Define server logic 
server <- function(input,output,session) {
  
  
  
    #Code for about page
    source("functions/about.R",local = TRUE)$value
    
    #code for validation of inputs
    source("functions/validation.R", local = TRUE)$value
    
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
    
    #Code for export UI
    source("functions/exportUI.R",local = TRUE)$value
    
    #Code for export processing
    source("functions/exportprocessing.R",local = TRUE)$value
    
    #Code for compiling metadata
    source("functions/metadata.R",local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)

#Future update ideas
# -change qc thresholds according to depth
# -indicate whether a logger model will be processed with multiple units, ie depths
# -allow the user to rearrange the export fields

#Notes

# -need to figure out how to properly configure ContDataQC, re: Air vs. Water
# -add saved notifications where needed to indicate something worked
# -add many notes and documentation