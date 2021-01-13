#Setup and Processing Tab----
#Interface for setting up the data for processing and processing
tabItems(
  #Home Tab----
  tabItem(
    tabName = "abouttab",
    useSweetAlert()
  ),
  #Programs and Waterbodies Tab----
  tabItem(
    tabName = "programsandwaterbodiestab",
    fluidRow(
      column(
        width = 7,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Load Configuration File",
          uiOutput("loadconfigfileUI")
        )
      ),
      column(
        width = 5,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("loadconfigfiledescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Add/Edit/Delete Programs",
          uiOutput("aedprogramsUI")
        )
      ),
      column(
        width = 5,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("aedprogramsdescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Add/Edit/Delete Waterbodies",
          uiOutput("aedwaterbodiesUI")
        )
      ),
      column(
        width = 5,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("aedwaterbodiesdescUI")
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Add/Edit/Delete Stations",
          uiOutput("aedstationsUI")
        )
      ),
      column(
        width = 5,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = ""
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
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Program",
          uiOutput("selectprogramsUI"),
        ),
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Waterbodies",
          uiOutput("selectwbsUI")
        ),
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Parameter",
          uiOutput("loggerconfigselectUI")
        )
      ),
      column(
        width = 6,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "QC Configuration",
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
          plotlyOutput("VisQCplot",height = 800)
        )
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