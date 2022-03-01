#Setup and Processing Tab----

#Interface for setting up the data for processing and processing
  tabItems(
  #About Tab----
  tabItem(
    tabName = "abouttab",
    uiOutput("aboutUI"),
    useSweetAlert()
  ),
  #Programs and Waterbodies Tab----
  tabItem(
    tabName = "programsandwaterbodiestab",
    fluidRow(
      column(
        width = 7,
        box(
          title = "Load Configuration File",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("loadconfigfileUI")
        )
      ),
      column(
        width = 5,
        box(
          title = "",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("loadconfigfiledescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          title = "Add/Edit/Delete Programs",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedprogramsUI")
        )
      ),
      column(
        width = 5,
        box(
          title = "",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedprogramsdescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          title = "Add/Edit/Delete Waterbodies",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedwaterbodiesUI")
        )
      ),
      column(
        width = 5,
        box(
          title = "",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedwaterbodiesdescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          title = "Add/Edit/Delete Stations",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedstationsUI")
        )
      ),
      
      column(
        width = 5,
        box(
          title = "",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("aedstationsdescUI")
        )
      )
    )
  ),
  #QC Configuration Tab----
  tabItem(
    
    tabName = "configqctab",
    fluidRow(
      column(
        width = 3,
        box(
          title = "Program",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("selectprogramsUI")
        ),
        box(
          title = "Waterbodies",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("selectwbsUI")
        ),
        box(
          title = "Parameter",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("loggerconfigselectUI")
        ),
        box(
          title = "QC Rules",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("loggerlevelsUI"),
          uiOutput("qcleveleditorUI")
          
        )
      ),
      column(
        width = 6,
        box(
          title = "QC Configuration",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          uiOutput("configUI")
        )
      ),
      column(
        width = 3,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("configdescUI")
        )
      )
    )
  ),
  #Logger File Definitions Tab----
  tabItem(
    tabName = "logfiledefstab",
    fluidRow(
      column(
        width = 9,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Manage Logger Models",
          collapsible = TRUE,
          collapsed = FALSE,
          uiOutput("aedoptionsUI")
        )
      ),
      column(
        width = 3,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("aedoptionsdescUI"),
        ),
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("lfloggerconfigdescUI")
        )
      )
    )
  ),
  #Export Options Tab----
  tabItem(
    tabName = "exportoptionstab",
    uiOutput("exportoptionsUI")
  ),
  #Save Config File Tab----
  tabItem(
    tabName = "saveconfigfiletab",
    fluidRow(
      column(
        width = 6,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Save Configuration File",
          uiOutput("saveconfigUI")
        )
      ),
      column(
        width = 6,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("saveconfigdescUI")
        )
      )
    )
  ),
  #Setup and Processing Tab----
  tabItem(
    tabName = "setuptab",
    uiOutput("pwlmUI"),
    fluidRow(
      column(
        width = 5,
        uiOutput("depthstableoutputUI")
      ),
      column(
        width = 4,
        box(
          title = "Upload logger data",
          solidHeader = TRUE,
          collapsible = FALSE,
          status = "primary",
          width = NULL,
          #Data upload input
          fileInput(
            inputId = "dataupload",
            label = "",
            multiple = TRUE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              ".TXT",
              ".txt"
            )
          ),
          #UI output for ui section rendered in the server side of the app
          uiOutput("procmetadataUI"),
          tags$br(),
          tags$hr(),
          tags$br(),
          progressBar(
            "processprogress",
            value = 0,
            display_pct = TRUE,
            status = "success",
            title = "Begin Processing"
          )
        )
      ),
      column(
        width = 3,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("procdescUI")
        )
      )
    ),
    box(
      title = "Data Preview",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      #Table output showing data processed up to this point
      uiOutput("dataprevUI")
    )
  ),
  
  #Visual QC tab----
  tabItem(
    tabName = "visqctab",
    fluidRow(
      column(
        width = 2,
        box(
          title = "QC Tools",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          #UI output of qc options rendered in the server side of the app
          uiOutput("visqcoptionsUI")
        ),
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "Description",
          collapsible = TRUE,
          collapsed = TRUE,
          uiOutput("visqcdescUI")
        )
      ),
      column(
        width = 10,
        box(
          title = "QC Plot",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          #output for Visual QC plotly graph
          uiOutput("VisQCplotUI")
        ),
        uiOutput("notesentryUI")
      )
    )
  ),
  #Summary tab----
  tabItem(
    tabName = "summarytab",
    uiOutput("summaryUI")
  ),
  #Export tab----
  tabItem(
    tabName = "exporttab",
    uiOutput("exportUI")
  )
)
# })