#Setup and Processing Tab----
#Interface for setting up the data for processing and processing
tabItems(
  #Home Tab----
  tabItem(
    tabName = "home",
    useSweetAlert()
  ),
  #Programs and Waterbodies Tab----
  tabItem(
    tabName = "programsandwaterbodies",
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
    tabName = "configqc",
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
          title = "Logger",
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
          # tableOutput("test")
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
    tabName = "logfiledefs",
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
  #Save Config File Tab----
  tabItem(
    tabName = "saveconfigfile",
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
    tabName = "setup",
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
          uiOutput("procmetadata"),
          actionBttn(
            inputId = "processing",
            label = "Process Data",
            color = "success",
            style = "fill",
            size = "lg",
            icon = icon("desktop")
          ),
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
        # box(
        #   title = "Progress",
        #   solidHeader = TRUE,
        #   status = "primary",
        #   width = NULL,
        #   #Processing progress bar
        #   progressBar(
        #     "processprogress",
        #     value = 0,
        #     display_pct = TRUE,
        #     status = "success"
        #   )
        # )
      )
    ),
    box(
      title = "Data Output",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      #Table output showing data processed up to this point
      DTOutput("dataoutput")
    )
  ),
  
  #Visual QC tab----
  tabItem(
    tabName = "visqc",
    fluidRow(
      column(
        width = 2,
        box(
          title = "QC Tools",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          #UI output of qc options rendered in the server side of the app
          uiOutput("visqcoptions")
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
  
  #Export tab----
  tabItem(
    tabName = "export",
    fluidRow(
      column(
        width = 3,
        box(
          title = "Export Options",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          #Picker input for selecting the final data format
          pickerInput(
            inputId = "exporttype",
            label = "Data Export Type",
            choices = c("All Data","Remove Failed Visual QC","Daily Summary")),
          actionBttn(
            inputId = "processexport",
            label = "Process Data Export",
            color = "success",
            style = "fill"
          ),
          tags$br(),
          tags$br(),
          #Download Button
          downloadButton("download")
        )
      ),
      column(
        width = 9,
        box(
          title = "Export Progress",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          #Final processing progress bar
          progressBar(
            "exportprogress",
            value = 0,
            display_pct = TRUE
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
      DTOutput("exportpreview")
    )
  )
)
