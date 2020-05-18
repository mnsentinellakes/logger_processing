output$rtsidebar=renderUI({
  configfiles=gsub(".R","",list.files("config"))
  configfiles=configfiles[configfiles != "default_orig"]
  
  #UI for ContDataQC thresholds in the rightsidebar
  rightSidebar(
    width = 350,
    rightSidebarTabContent(
      id = "fileselect",
      title = "Config File",
      icon = "file",
      active = TRUE,
      pickerInput(
        inputId = "configfile",
        label = "QC Configuration File",
        choices = configfiles,
        selected = "default"
      ),
      actionBttn(
        inputId = "loadconfig",
        label = "Load",
        style = "fill",
        color = "success",
        icon = icon("dolly")
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      textInput(
        inputId = "configsavename",
        label = "Save Configuration File",
        placeholder = "File Name"
      ),
      actionBttn(
        inputId = "saveconfig",
        label = "Save",
        style = "fill",
        color = "danger",
        icon = icon("save")
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      actionBttn(
        inputId = "restoredefault",
        label = "Restore Default",
        style = "fill",
        color = "warning",
        icon = icon("sync")
      )
    ),
    rightSidebarTabContent(
      id = "temperature",
      title = "Temperature Settings",
      icon = "thermometer-half",
      uiOutput("tempthresholds")
    ),
    rightSidebarTabContent(
      id = "dissolvedoxygen",
      title = "Dissolved Oxygen Settings",
      icon = "tint",
      uiOutput("dothresholds")
    )
  )
})

#Temperature Threshold UI
output$tempthresholds=renderUI({
  tagList(
  tags$h4("Gross Range Thresholds"),
  tags$h6("Test if data point exceeds sensor or user defined min/max."),
  HTML("<CENTER>"),
  fluidRow(
    column(
      width = 6,
      tags$h5("Fail")
    ),
    column(
      width = 6,
      tags$h5("Suspect")
    )
  ),
  fluidRow(
    column(
      width = 3,
      textInput(
        inputId = "tempgrossmaxfail",
        label = "Max",
        width = 55,
        value = ContData.env$myThresh.Gross.Fail.Hi.WaterTemp
      ),
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempgrossminfail",
        label = "Min",
        width = 55,
        value = ContData.env$myThresh.Gross.Fail.Lo.WaterTemp
      )
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempgrossmaxsusp",
        label = "Max",
        width = 55,
        value = ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp
      )
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempgrossminsusp",
        label = "Min",
        width = 55,
        value = ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp
      )
    )
  ),
  HTML("</CENTER>"),
  tags$h4("Spike Thresholds"),
  tags$h6("Test if data point exceeds a user defined threshold relative to the previous data point."),
  HTML("<CENTER>"),
  fluidRow(
    column(
      width = 3,
      textInput(
        inputId = "tempspikemaxfail",
        label = "Max",
        width = 55,
        value = ContData.env$myThresh.Spike.Hi.WaterTemp
      ),
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempspikeminfail",
        label = "Min",
        width = 55,
        value = ContData.env$myThresh.Spike.Lo.WaterTemp
      )
    )
  ),
  HTML("</CENTER>"),
  tags$h4("Rate of Change Limits"),
  tags$h6("Test if a data point exceeds a number of standard deviations from the previous data points over a user defined time period."),
  HTML("<CENTER>"),
  fluidRow(
    column(
      width = 3,
      textInput(
        inputId = "temprocsd",
        label = "SDs",
        width = 55,
        value = ContData.env$myThresh.RoC.SD.number.WaterTemp
      ),
    ),
    column(
      width = 3,
      textInput(
        inputId = "temprocperiod",
        label = "Hours",
        width = 55,
        value = ContData.env$myThresh.RoC.SD.period.WaterTemp
      )
    )
  ),
  HTML("</CENTER>"),
  tags$h4("Flat Line Test"),
  tags$h6("Test if a data point is within a user defined threshold from previous data points over a user defined range."),
  HTML("<CENTER>"),
  fluidRow(
    column(
      width = 3,
      textInput(
        inputId = "tempflathi",
        label = "Max",
        width = 55,
        value = ContData.env$myThresh.Flat.Hi.WaterTemp
      ),
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempflatlow",
        label = "Min",
        width = 55,
        value = ContData.env$myThresh.Flat.Lo.WaterTemp
      )
    ),
    column(
      width = 3,
      textInput(
        inputId = "tempflattol",
        label = "Tolerance",
        width = 55,
        value = ContData.env$myThresh.Flat.Tolerance.WaterTemp
      )
    )
  ),
  HTML("</CENTER>")
)
})

#DO thresholds UI
output$dothresholds=renderUI({
  tagList(
    tags$h4("Gross Range Thresholds"),
    tags$h6("Test if data point exceeds sensor or user defined min/max."),
    HTML("<CENTER>"),
    fluidRow(
      column(
        width = 6,
        tags$h5("Fail")
      ),
      column(
        width = 6,
        tags$h5("Suspect")
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput(
          inputId = "dogrossmaxfail",
          label = "Max",
          width = 55,
          value = ContData.env$myThresh.Gross.Fail.Hi.DO
        ),
      ),
      column(
        width = 3,
        textInput(
          inputId = "dogrossminfail",
          label = "Min",
          width = 55,
          value = ContData.env$myThresh.Gross.Fail.Lo.DO
        )
      ),
      column(
        width = 3,
        textInput(
          inputId = "dogrossmaxsusp",
          label = "Max",
          width = 55,
          value = ContData.env$myThresh.Gross.Suspect.Hi.DO
        )
      ),
      column(
        width = 3,
        textInput(
          inputId = "dogrossminsusp",
          label = "Min",
          width = 55,
          value = ContData.env$myThresh.Gross.Suspect.Lo.DO
        )
      )
    ),
    HTML("</CENTER>"),
    tags$h4("Spike Thresholds"),
    tags$h6("Test if data point exceeds a user defined threshold relative to the previous data point."),
    HTML("<CENTER>"),
    fluidRow(
      column(
        width = 3,
        textInput(
          inputId = "dospikemaxfail",
          label = "Max",
          width = 55,
          value = ContData.env$myThresh.Spike.Hi.DO
        ),
      ),
      column(
        width = 3,
        textInput(
          inputId = "dospikeminfail",
          label = "Min",
          width = 55,
          value = ContData.env$myThresh.Spike.Lo.DO
        )
      )
    ),
    HTML("</CENTER>"),
    tags$h4("Rate of Change Limits"),
    tags$h6("Test if a data point exceeds a number of standard deviations from the previous data points over a user defined time period."),
    HTML("<CENTER>"),
    fluidRow(
      column(
        width = 3,
        textInput(
          inputId = "dorocsd",
          label = "SDs",
          width = 55,
          value = ContData.env$myThresh.RoC.SD.number.DO
        ),
      ),
      column(
        width = 3,
        textInput(
          inputId = "dorocperiod",
          label = "Hours",
          width = 55,
          value = ContData.env$myThresh.RoC.SD.period.DO
        )
      )
    ),
    HTML("</CENTER>"),
    tags$h4("Flat Line Test"),
    tags$h6("Test if a data point is within a user defined threshold from previous data points over a user defined range."),
    HTML("<CENTER>"),
    fluidRow(
      column(
        width = 3,
        textInput(
          inputId = "doflathi",
          label = "Max",
          width = 55,
          value = ContData.env$myThresh.Flat.Hi.DO
        ),
      ),
      column(
        width = 3,
        textInput(
          inputId = "doflatlow",
          label = "Min",
          width = 55,
          value = ContData.env$myThresh.Flat.Lo.DO
        )
      ),
      column(
        width = 3,
        textInput(
          inputId = "doflattol",
          label = "Tolerance",
          width = 55,
          value = ContData.env$myThresh.Flat.Tolerance.DO
        )
      )
    ),
    HTML("</CENTER>")
  )
})