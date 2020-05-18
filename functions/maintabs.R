#Setup and Processing Tab----
#Interface for setting up the data for processing and processing
tabItems(
  tabItem(
    #Setup tab where serial numbers and their associated depths are added, deleted, or reset
    tabName = "setup",
    fluidRow(
      column(
        width = 3,
        box(
          title = "Depths Input",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          width = NULL,
          HTML("<CENTER>"),
          fluidRow(
            column(
              width = 6,
              #Serial Number Text Input
              textInput(
                inputId = "inputsn",
                label = "Serial Number"
              )
            ),
            column(
              width = 6,
              #Depth Text Input
              textInput(
                inputId = "inputdepth",
                label = "Depth"
              )
            )
          ),
          #Add serial number and depth data to lookup table
          actionBttn(
            inputId = "add",
            label = "Add",
            color = "success",
            style = "fill",
            icon = icon("plus")
          ),
          HTML("</CENTER>"),
          useSweetAlert()
        ),
        box(
          title = "Delete/Reset Rows",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          width = NULL,
          HTML("<CENTER>"),
          #Text input for row number to be deleted or reset
          textInput(
            inputId = "RowID",
            label = "RowID",
            width = 60),
          fluidRow(
            HTML("<CENTER>"),
            column(
              width = 6,
              #Delete row indicated in text input
              actionBttn(
                inputId = "delete",
                label = "Delete",
                color = "danger",
                style = "fill",
                icon = icon("minus")
              )
            ),
            column(
              width = 6,
              #Reset processed status of row indicated in the text input
              actionBttn(
                inputId = "reset",
                label = "Reset",
                color = "danger",
                style = "fill",
                icon = icon("redo")
              )
            )
          )
        ),
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
          uiOutput("loggerstring"),
          actionBttn(
            inputId = "processing",
            label = "Process Data",
            color = "success",
            style = "fill",
            icon = icon("desktop")
          )
        )
      ),
      column(
        width = 7,
        box(
          title = "Depths Table",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          width = NULL,
          HTML("<CENTER>"),
          #Table output for lookup table
          DTOutput("depthstableoutput"),
          HTML("</CENTER>"),
          textOutput("testx")
        )
      )
    ),
    box(
      title = "Progress",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      #Processing progress bar
      progressBar(
        "processprogress",
        value = 0,
        display_pct = TRUE,
        status = "success"
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
          plotlyOutput("VisQCplot")
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