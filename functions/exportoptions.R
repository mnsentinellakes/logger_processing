#Main Export UI
output$exportoptionsUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 3,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Program",
          uiOutput("selectprogramsexportUI"),
        ),
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Logger Model",
          uiOutput("selectloggerexportUI")
        )
      ),
      column(
        width = 6,
        box(
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          title = "Export Settings",
          uiOutput("exportsettingsUI")
        )
      ),
      column(
        width = 3,
        box(
          solidHeader = TRUE,
          status = "success",
          width = NULL,
          title = "",
          uiOutput("exportoptionsdescUI")
        )
      )
    )
  )
})

#Render the UI for selecting the program
output$selectprogramsexportUI = renderUI({
  pickerInput(
    inputId = "selectedprogramexport",
    label = NULL,
    #programid() can be found in the qcconfig.R file
    choices = programid()
  )
})

#Logger Models
loggermodelsexport = reactive({
  loggermodelselect = loggerfiledefs()
  loggermodelselectids = loggermodelselect$ModelID
  names(loggermodelselectids) = loggermodelselect$Logger_Model
  
  return(loggermodelselectids)
})

#Select Logger Model UI
output$selectloggerexportUI = renderUI({
  pickerInput(
    inputId = "selectloggerexport",
    label = NULL,
    choices = loggermodelsexport()
  )
})

#Select appropriate row from the export table
exportselect = reactive({
  namesdeploy = export()
  namesdeploy = namesdeploy[which(namesdeploy$ProgramID == input$selectedprogramexport & namesdeploy$ModelID == input$selectloggerexport),]
  
  return(namesdeploy)
})

#Export Settings UI
output$exportsettingsUI = renderUI({
  supportfilesexport = exportselect()
  
  if (nrow(supportfilesexport) > 0){
    sepfilevalue = supportfilesexport$FileSep
    notesvalue = supportfilesexport$IncNotes
    metavalue = supportfilesexport$IncMeta
    reportvalue = supportfilesexport$IncRep
    configvalue = supportfilesexport$IncConfig
    summaryvalue = supportfilesexport$IncSum
  }else{
    sepfilevalue = "Single"
    notesvalue = FALSE
    metavalue = TRUE
    reportvalue = TRUE
    configvalue = TRUE
    summaryvalue = TRUE
  }
  
  tagList(
    fluidRow(
      column(
        width = 3,
        tags$div(
          style = "vertical-align:center; border:1px solid lightgray; padding-right:10px; padding-left:10px; background-color:ghostwhite; 
          width: 100%",
          tags$h4("Files"),
          awesomeRadio(
            inputId = "sepfile",
            label = NULL,
            choices = c("Single","Multiple"),
            selected = sepfilevalue,
            status = "success"
          )
        )
      ),
      column(
        width = 3,
        style = "vertical-align:center; border:1px solid lightgray; padding-right:10px; padding-left:10px; background-color:ghostwhite;",
        tags$h4("Notes"),
        prettyCheckbox(
          inputId = "incnotes",
          label = "Include QC Notes",
          value = notesvalue,
          status = "success"
        )
      )
    ),
    tags$br(),
    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "vertical-align:center; border:1px solid lightgray; padding-right:10px; padding-left:10px; background-color:ghostwhite; 
          width: 100%",
          tags$h4("Include Support Files"),
          fluidRow(
            column(
              width = 3,
              prettyCheckbox(
                inputId = "incmeta",
                label = "Metadata",
                value = metavalue,
                status = "success"
              )
            ),
            column(
              width = 3,
              prettyCheckbox(
                inputId = "increp",
                label = "Report",
                value = reportvalue,
                status = "success"
              )
            ),
            column(
              width = 3,
              prettyCheckbox(
                inputId = "incconfig",
                label = "Config",
                value = configvalue,
                status = "success"
              )
            ),
            column(
              width = 3,
              prettyCheckbox(
                inputId = "incsummary",
                label = "Summary",
                value = summaryvalue,
                status = "success"
              )
            )
          )
        )
      )
    ),
    tags$br(),
    tags$div(
      style = "vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 3,
          tags$h4("Identification Field Names")
        ),
        column(
          width = 9,
          uiOutput("namesdeployexportUI")
        )
      )
    ),
    tags$br(),
    tags$div(
      style = "vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 3,
          tags$h4("Location Field Names")
        ),
        column(
          width = 9,
          uiOutput("locationexportUI")
        )
      )
    ),
    tags$br(),
    tags$div(
      style = "vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 3,
          tags$h4("Date and Time Field Names")
        ),
        column(
          width = 9,
          fluidRow(
            column(
              width = 5,
              uiOutput("datefieldexportUI")
            ),
            column(
              width = 7,
              uiOutput("datetimeexportUI"),
              uiOutput("timesettingsexportUI")
            )
          )
        )
      )
    ),
    tags$br(),
    tags$div(
      style = "vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 3,
          tags$h4("Logger Data Field Names")
        ),
        column(
          width = 9,
          uiOutput("exportdatafieldsUI")
        )
      )
    ),
    tags$br(),
    actionBttn(
      inputId = "saveexportoptionsbttn",
      label = "Save Export Settings",
      style = "fill",
      color = "success",
      size = "md"
    )
  )
})

#Waterbody Names and Deployment Information UI
output$namesdeployexportUI = renderUI({
  namesdeployfieldvalues = exportselect()
  
  if (nrow(namesdeployfieldvalues) > 0){
    incmodelnamevalue = namesdeployfieldvalues$IncModelName
    incprogramnamevalue = namesdeployfieldvalues$IncProgramName
    incwbidvalue = namesdeployfieldvalues$IncProgramWBID
    incwbnamevalue = namesdeployfieldvalues$IncWBName
    incwbtypevalue = namesdeployfieldvalues$IncWBType
    incstationidvalue = namesdeployfieldvalues$IncProgramStationID
    incstationnamevalue = namesdeployfieldvalues$IncStationName
    incunitidvalue = namesdeployfieldvalues$IncUnitID
    incdeploymentvalue = namesdeployfieldvalues$IncDeploy
    incusernamevalue = namesdeployfieldvalues$IncUser
  }else{
    incmodelnamevalue = TRUE
    incprogramnamevalue = TRUE
    incwbidvalue = TRUE
    incwbnamevalue = TRUE
    incwbtypevalue = TRUE
    incstationidvalue = TRUE
    incstationnamevalue = TRUE
    incunitidvalue = TRUE
    incdeploymentvalue = TRUE
    incusernamevalue = TRUE
  }
  
  tagList(
    fluidRow(
      column(
        width = 4,
      ),
      column(
        width = 8,
        uiOutput("unitidexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incmodelnameexport",
          label = "Model Name",
          value = incmodelnamevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("modelnameexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incprogramnameexport",
          label = "Program Name",
          value = incprogramnamevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("programnameexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incwbidexport",
          label = "Waterbody ID",
          value = incwbidvalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("wbidexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incwbnameexport",
          label = "Waterbody Name",
          value = incwbnamevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("wbnameexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incwbtypeexport",
          label = "Waterbody Type",
          value = incwbtypevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("wbtypeexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incstationidexport",
          label = "Station ID",
          value = incstationidvalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("stationidexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incstationnameexport",
          label = "Station Name",
          value = incstationnamevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("stationnameexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incdeploymentexport",
          label = "Deployment",
          value = incdeploymentvalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("deploymentexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "incusernameexport",
          label = "User Name",
          value = incusernamevalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("usernameexportUI")
      )
    )
  )
})

#Unit ID Field UI
output$unitidexportUI = renderUI({
  incunitidexportvalue = exportselect()
  
  textInput(
    inputId = "unitidexport",
    label = "Unit ID Field Name",
    value = incunitidexportvalue$UnitID
  )
})

#Model Name Field Name UI
output$modelnameexportUI = renderUI({
  incmodelnameexportvalue = exportselect()
  if (input$incmodelnameexport == TRUE){
    textInput(
      inputId = "modelnameexport",
      label = "Model Name Field Name",
      value = incmodelnameexportvalue$ModelName
    )
  }else{}
})

#Program Name Field Name UI
output$programnameexportUI = renderUI({
  incprogramnameexportvalue = exportselect()
  if (input$incprogramnameexport == TRUE){
    textInput(
      inputId = "programnameexport",
      label = "Program Name Field Name",
      value = incprogramnameexportvalue$ProgramName
    )
  }else{}
})

#WBID Field Name UI
output$wbidexportUI = renderUI({
  incwbidexportvalue = exportselect()
  if (input$incwbidexport == TRUE){
    textInput(
      inputId = "wbidexport",
      label = "Waterbody ID Field Name",
      value = incwbidexportvalue$ProgramWBID
    )
  }else{}
})

#WB Name Field Name UI
output$wbnameexportUI = renderUI({
  incwbnameexportvalue = exportselect()
  if (input$incwbnameexport == TRUE){
    textInput(
      inputId = "wbnameexport",
      label = "Waterbody Name Field Name",
      value = incwbnameexportvalue$WBName
    )
  }else{}
})

#WB Type Field Name UI
output$wbtypeexportUI = renderUI({
  incwbtypeexportvalue = exportselect()
  if (input$incwbtypeexport == TRUE){
    textInput(
      inputId = "wbtypeexport",
      label = "Waterbody Type Field Name",
      value = incwbtypeexportvalue$WBType
    )
  }else{}
})

#Station ID Field UI
output$stationidexportUI = renderUI({
  incstationidexportvalue = exportselect()
  if (input$incstationidexport == TRUE){
    textInput(
      inputId = "stationidexport",
      label = "Station ID Field Name",
      value = incstationidexportvalue$ProgramStationID
    )
  }else{}
})

#Station Name Field UI
output$stationnameexportUI = renderUI({
  incstationnameexportvalue = exportselect()
  if (input$incstationnameexport == TRUE){
    textInput(
      inputId = "stationnameexport",
      label = "Station Name Field Name",
      value = incstationnameexportvalue$StationName
    )
  }else{}
})

#Deployment Field UI
output$deploymentexportUI = renderUI({
  incdeploymentexportvalue = exportselect()
  if (input$incdeploymentexport == TRUE){
    textInput(
      inputId = "deploymentexport",
      label = "Deployment Field Name",
      value = incdeploymentexportvalue$Deployment
    )
  }else{}
})

#User Name Field UI
output$usernameexportUI = renderUI({
  incusernameexportvalue = exportselect()
  if (input$incusernameexport == TRUE){
    textInput(
      inputId = "usernameexport",
      label = "User Name Field Name",
      value = incusernameexportvalue$User
    )
  }else{}
})

#Location Field Names UI
output$locationexportUI = renderUI({
  locationfieldvalues = exportselect()
  if (nrow(locationfieldvalues) > 0){
    inczvalue = locationfieldvalues$IncZ
    inclocvalue = locationfieldvalues$IncLoc
  }else{
    inczvalue = TRUE
    inclocvalue = TRUE
  }
  
  tagList(
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "inczexport",
          label = "Z data",
          value = inczvalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("zexportUI")
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$br(),
        prettyCheckbox(
          inputId = "inclocexport",
          label = "Coordinates",
          value = inclocvalue,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("locexportUI")
      )
    )
  )
})

#Z Field UI
output$zexportUI = renderUI({
  inczexportvalue = exportselect()
  if (input$inczexport == TRUE){
    textInput(
      inputId = "zexport",
      label = "Z Data Field Name",
      value = inczexportvalue$Z
    )
  }else{}
})

#X and Y Fields
output$locexportUI = renderUI({
  inclocexportvalue = exportselect()
  if (input$inclocexport == TRUE){
    tagList(
      textInput(
        inputId = "latexport",
        label = "Latitude Field Name",
        value = inclocexportvalue$Lat
      ),
      textInput(
        inputId = "lonexport",
        label = "Longitude Field Name",
        value = inclocexportvalue$Lon
      )
    )
  }else{}
})

#Add Logger Date Fields
output$datefieldexportUI = renderUI({
  datetypeexportvalue = exportselect()
  
  if (nrow(datetypeexportvalue) > 0){
    datetypevalue = datetypeexportvalue$DateTimeSep
  }else{
    datetypevalue = "Combined"
  }
  
  radioGroupButtons(
    inputId = "datetypeexport",
    label = "Field Organization",
    choices = c("Combined","Separate"),
    selected = datetypevalue,
    status = "primary"
  )
})

#Date Time Fields Main UI
output$datetimeexportUI = renderUI({
  validate(
    need(input$datetypeexport,"Loading...")
  )
  datetimecolexportvalue = exportselect()
  
  if (input$datetypeexport == "Combined"){
    fluidRow(
      column(
        width = 10,
        textInput(
          inputId = "datetimecombinecolexport",
          label = "Date Time Field Name",
          value = datetimecolexportvalue$Date_Time
        )
      )
    )
  }else{
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "datecolexport",
          label = "Date Field Name",
          value = datetimecolexportvalue$Date
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = "timecolexport",
          label = "Time Field Name",
          datetimecolexportvalue$Time
        )
      )
    )
  }
})

#Date Time UI
output$timesettingsexportUI = renderUI({
  tzexportvalue = exportselect()
  
  fluidRow(
    column(
      width = 10,
      pickerInput(
        inputId = "tzexport",
        label = "Time Zone",
        choices = OlsonNames(),
        selected = tzexportvalue$TZ
      )
    )
  )
})

#Select Logger Types in model
modelnamesselectexport = reactive({
  modelexport = loggerfiledefs()
  
  modelexport = modelexport[which(modelexport$ModelID == input$selectloggerexport),]
  modelexport = modelexport[,8:18]
  modelexportnona = modelexport %>% select_if(function(x){all(!is.na(x)) & all(x != "")})
  
  loggermodeltypes = colnames(modelexportnona)
  
  return(loggermodeltypes)
})

#Assign Field Names to appropriate Fields
storedexportnames = reactive({
  exportnames = export()
  exportnames = exportnames[which(exportnames$ProgramID == input$selectedprogramexport & exportnames$ModelID == input$selectloggerexport),]
  
  exportnameslist = NULL
  if ("AirBP" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"AirBP" = exportnames$AirBP)
    }else{
      exportnameslist = c(exportnameslist,"AirBP" = NA)
    }
  }
  if("AirTemp" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"AirTemp" = exportnames$AirTemp)
    }else{
      exportnameslist = c(exportnameslist,"AirTemp" = NA)
    }
  }
  
  if ("Chlorophylla" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"Chlorophylla" = exportnames$Chlorophylla)
    }else{
      exportnameslist = c(exportnameslist,"Chlorophylla" = NA)
    }
  }
  
  if ("Cond" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"Cond" = exportnames$Cond)
    }else{
      exportnameslist = c(exportnameslist,"Cond" = NA)
    }
  }
  
  if ("Discharge" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"Discharge" = exportnames$Discharge)
    }else{
      exportnameslist = c(exportnameslist,"Discharge" = NA)
    }
  }
  
  if ("DO" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"DO" = exportnames$DO)
    }else{
      exportnameslist = c(exportnameslist,"DO" = NA)
    }
  }
  
  if ("pH" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"pH" = exportnames$pH)
    }else{
      exportnameslist = c(exportnameslist,"pH" = NA)
    }
  }
  
  if ("Turbidity" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"Turbidity" = exportnames$Turbidity)
    }else{
      exportnameslist = c(exportnameslist,"Turbidity" = NA)
    }
  }
  
  if ("WaterLevel" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"WaterLevel" = exportnames$WaterLevel)
    }else{
      exportnameslist = c(exportnameslist,"WaterLevel" = NA)
    }
  }
  
  if ("WaterP" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"WaterP" = exportnames$WaterP)
    }else{
      exportnameslist = c(exportnameslist,"WaterP" = NA)
    }
  }
  
  if ("WaterTemp" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"WaterTemp" = exportnames$WaterTemp)
    }else{
      exportnameslist = c(exportnameslist,"WaterTemp" = NA)
    }
  }
  
  return(exportnameslist)
})

#Data Field Names UI
output$exportdatafieldsUI = renderUI({
  
  exportfieldnames = storedexportnames()
  
  tagList(
    if ("AirBP" %in% names(exportfieldnames)){
      textInput(
        inputId = "airbpexport",
        label = "AirBP Field Name",
        value = exportfieldnames["AirBP"]
      )
    },
    if ("AirTemp" %in% names(exportfieldnames)){
      textInput(
        inputId = "airtempexport",
        label = "AirTemp Field Name",
        value = exportfieldnames["AirTemp"]
      )
    },
    if ("Chlorophylla" %in% names(exportfieldnames)){
      textInput(
        inputId = "chlorophyllaexport",
        label = "Chlorophylla Field Name",
        value = exportfieldnames["Chlorophylla"]
      )
    },
    if ("Cond" %in% names(exportfieldnames)){
      textInput(
        inputId = "condexport",
        label = "Cond Field Name",
        value = exportfieldnames["Cond"]
      )
    },
    if ("Discharge" %in% names(exportfieldnames)){
      textInput(
        inputId = "dischargeexport",
        label = "Discharge Field Name",
        value = exportfieldnames["Discharge"]
      )
    },
    if ("DO" %in% names(exportfieldnames)){
      textInput(
        inputId = "doexport",
        label = "DO Field Name",
        value = exportfieldnames["DO"]
      )
    },
    if ("pH" %in% names(exportfieldnames)){
      textInput(
        inputId = "phexport",
        label = "pH Field Name",
        value = exportfieldnames["pH"]
      )
    },
    if ("Turbidity" %in% names(exportfieldnames)){
      textInput(
        inputId = "turbidityexport",
        label = "Turbidity Field Name",
        value = exportfieldnames["Turbidity"]
      )
    },
    if ("WaterLevel" %in% names(exportfieldnames)){
      textInput(
        inputId = "waterlevelexport",
        label = "WaterLevel Field Name",
        value = exportfieldnames["WaterLevel"]
      )
    },
    if ("WaterP" %in% names(exportfieldnames)){
      textInput(
        inputId = "waterpexport",
        label = "WaterP Field Name",
        value = exportfieldnames["WaterP"]
      )
    },
    if ("WaterTemp" %in% names(exportfieldnames)){
      textInput(
        inputId = "watertempexport",
        label = "WaterTemp Field Name",
        value = exportfieldnames["WaterTemp"]
      )
    }
  )
})

#Observe saveexportoptions button and save to table
observeEvent(
  input$saveexportoptionsbttn,{
    exportfinal = export()
    loggerexportfinal = loggerfiledefs()
    loggerexportfinal = loggerexportfinal[which(loggerexportfinal$ModelID == input$selectloggerexport),]
    
    #Number of Checked Column Names
    nchecked = sum(c(input$incmodelnameexport,input$incprogramnameexport,input$incwbidexport,input$incwbnameexport,input$incwbtypeexport,
                     input$incstationidexport,input$incstationnameexport,input$incdeploymentexport,input$incusernameexport,input$inczexport,input$inclocexport))
    
    #Number of Column Names with Name filled out
    ncomplete = sum(c(input$modelnameexport != "",input$programnameexport != "",input$wbidexport != "",input$wbnameexport != "",
                      input$wbtypeexport != "",input$stationidexport != "",input$stationnameexport != "",input$deploymentexport != "",
                      input$usernameexport != "",input$zexport != "",input$latexport != ""))
    
    message(paste0("Include Model Name: ",input$incmodelnameexport,";\n Include Program Name: ",input$incprogramnameexport,";\n Include Waterbody ID: ",
                   input$incwbidexport,";\n Include Waterbody Name: ",input$incwbnameexport,";\n Include Waterbody Type: ",input$incwbtypeexport,
                   ";\n Include Station ID: ",input$incstationidexport,";\n Include Station Name: ",input$incstationnameexport,";\n Include Deployment Count: ",
                   input$incdeploymentexport,";\n Include User Name: ",input$incusernameexport,";\n Include Z Value: ",input$inczexport,";\n Include Location: ",
                   input$inclocexport,";"))
    
    message(paste0(nchecked," field names selected for inclusion"))
    message(paste0(ncomplete," field names with names"))
    if (nchecked == ncomplete){
      #If settings already exist for the program and model combination
      if (nrow(exportselect()) > 0){
        #File Setup
        exportfinal$FileSep[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$sepfile
        exportfinal$IncNotes[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incnotes
        exportfinal$IncMeta[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incmeta
        exportfinal$IncRep[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$increp
        exportfinal$IncConfig[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incconfig
        exportfinal$IncSum[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incsummary
        
        #Identification Field Names
        exportfinal$UnitID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$unitidexport
        
        exportfinal$IncModelName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incmodelnameexport
        if (input$incmodelnameexport == TRUE){
          exportfinal$ModelName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$modelnameexport
        }else{
          exportfinal$ModelName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncProgramName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incprogramnameexport
        if (input$incprogramnameexport == TRUE){
          exportfinal$ProgramName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$programnameexport
        }else{
          exportfinal$ProgramName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncProgramWBID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incwbidexport
        if (input$incwbidexport == TRUE){
          exportfinal$ProgramWBID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$wbidexport
        }else{
          exportfinal$ProgramWBID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncWBName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incwbnameexport
        if (input$incwbnameexport == TRUE){
          exportfinal$WBName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$wbnameexport
        }else{
          exportfinal$WBName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncWBType[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incwbtypeexport
        if (input$incwbtypeexport == TRUE){
          exportfinal$WBType[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$wbtypeexport
        }else{
          exportfinal$WBType[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncProgramStationID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incstationidexport
        if (input$incstationidexport == TRUE){
          exportfinal$ProgramStationID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$stationidexport  
        }else{
          exportfinal$ProgramStationID[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncStationName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incstationnameexport
        if (input$incstationnameexport == TRUE){
          exportfinal$StationName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$stationnameexport
        }else{
          exportfinal$StationName[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncDeploy[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incdeploymentexport
        if (input$incdeploymentexport == TRUE){
          exportfinal$Deployment[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$deploymentexport
        }else{
          exportfinal$Deployment[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncUser[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$incusernameexport
        if (input$incusernameexport == TRUE){
          exportfinal$User[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$usernameexport
        }else{
          exportfinal$User[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        #Location Field Names
        exportfinal$IncZ[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$inczexport
        if (input$inczexport == TRUE){
          exportfinal$Z[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$zexport
        }else{
          exportfinal$Z[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        exportfinal$IncLoc[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$inclocexport
        if (input$inclocexport == TRUE){
          exportfinal$Lat[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$latexport
          exportfinal$Lon[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$lonexport
        }else{
          exportfinal$Lat[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
          exportfinal$Lon[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        #Date and Time Field Names
        exportfinal$DateTimeSep[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$datetypeexport
        if (input$datetypeexport == "Combined"){
          message("Date and Time fields will be combined in the export")
          exportfinal$Date_Time[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$datetimecombinecolexport
          exportfinal$Date[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
          exportfinal$Time[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }else if (input$datetypeexport == "Separate"){
          message("Date and Time fields will be separate in the export")
          exportfinal$Date_Time[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
          exportfinal$Date[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$datecolexport
          exportfinal$Time[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$timecolexport
        }
        
        exportfinal$TZ[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$tzexport
        
        #Data Field Names
        if (!is.na(loggerexportfinal$AirBP) & loggerexportfinal$AirBP != ""){
          message(paste0("The AirBP field will be in the export and is named ",input$airbpexport))
          exportfinal$AirBP[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$airbpexport
        }else{
          message("The AirBP field will not be included in the export")
          exportfinal$AirBP[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$AirTemp) & loggerexportfinal$AirTemp != ""){
          message(paste0("The AirTemp field will be in the export and is named ",input$airtempexport))
          exportfinal$AirTemp[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$airtempexport
        }else{
          message("The AirTemp field will not be included in the export")
          exportfinal$AirTemp[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$Chlorophylla) & loggerexportfinal$Chlorophylla != ""){
          message(paste0("The Chlorophylla field will be in the export and is named ",input$chlorophyllaexport))
          exportfinal$Chlorophylla[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$chlorophyllaexport
        }else{
          message("The Chlorophylla field will not be included in the export")
          exportfinal$Chlorophylla[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$Cond) & loggerexportfinal$Cond != ""){
          message(paste0("The Cond field will be in the export and is named ",input$condexport))
          exportfinal$Cond[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$condexport
        }else{
          message("The Cond field will not be included in the export")
          exportfinal$Cond[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$Discharge) & loggerexportfinal$Discharge != ""){
          message(paste0("The Discharge field will be in the export and is named ",input$dischargeexport))
          exportfinal$Discharge[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$dischargeexport
        }else{
          message("The Discharge field will not be included in the export")
          exportfinal$Discharge[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        if (!is.na(loggerexportfinal$DO) & loggerexportfinal$DO != ""){
          message(paste0("The DO field will be included in the export and is named ",input$doexport))
          exportfinal$DO[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$doexport
        }else{
          message("The DO field will not be included in the export")
          exportfinal$DO[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$pH) & loggerexportfinal$pH != ""){
          message(paste0("The pH field will be in the export and is named ",input$phexport))
          exportfinal$pH[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$phexport
        }else{
          message("The pH field will not be included in the export")
          exportfinal$pH[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$Turbidity) & loggerexportfinal$Turbidity != ""){
          message(paste0("The Turbidity field will be in the export and is named ",input$turbidityexport))
          exportfinal$Turbidity[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$turbidityexport
        }else{
          message("The Turbidity field will not be included in the export")
          exportfinal$Turbidity[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$WaterLevel) & loggerexportfinal$WaterLevel != ""){
          message(paste0("The WaterLevel field will be in the export and is named ",input$waterlevelexport))
          exportfinal$WaterLevel[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$waterlevelexport
        }else{
          message("The WaterLevel field will not be included in the export")
          exportfinal$WaterLevel[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$WaterP) & loggerexportfinal$WaterP != ""){
          message(paste0("The WaterP field will be in the export and is named ",input$waterpexport))
          exportfinal$WaterP[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$waterpexport
        }else{
          message("The WaterP field will not be included in the export")
          exportfinal$WaterP[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        if (!is.na(loggerexportfinal$WaterTemp) & loggerexportfinal$WaterTemp != ""){
          message(paste0("The WaterTemp field will be in the export and is named ",input$watertempexport))
          exportfinal$WaterTemp[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = input$watertempexport
        }else{
          message("The WaterTemp field will not be included in the export")
          exportfinal$WaterTemp[which(exportfinal$ProgramID == input$selectedprogramexport & exportfinal$ModelID == input$selectloggerexport)] = NA
        }
        
        #If settings do not exist for Program and Logger Model combination
      }else{
        unitidfinal = input$unitidexport
        
        if (input$incmodelnameexport == TRUE){
          modelnamefinal = input$modelnameexport
        }else{
          modelnamefinal = NA
        }
        
        if (input$incprogramnameexport == TRUE){
          programnamefinal = input$programnameexport
        }else{
          programnamefinal = NA
        }
        
        if (input$incwbidexport == TRUE){
          wbidfinal = input$wbidexport
        }else{
          wbidfinal = NA
        }
        
        if (input$incwbnameexport == TRUE){
          wbnamefinal = input$wbnameexport
        }else{
          wbnamefinal = NA
        }
        
        if (input$incwbtypeexport == TRUE){
          wbtypefinal = input$wbtypeexport
        }else{
          wbtypefinal = NA
        }
        
        if (input$incstationidexport == TRUE){
          stationidfinal = input$stationidexport
        }else{
          stationidfinal = NA
        }
        
        if (input$incstationnameexport == TRUE){
          stationnamefinal = input$stationnameexport
        }else{
          stationnamefinal = NA
        }
        
        if (input$incdeploymentexport == TRUE){
          deploymentfinal = input$deploymentexport
        }else{
          deploymentfinal = NA
        }
        
        if (input$incusernameexport == TRUE){
          usernamefinal = input$usernameexport
        }else{
          usernamefinal = NA
        }
        
        #Location Fields
        if (input$inczexport == TRUE){
          zfinal = input$zexport
        }else{
          zfinal = NA
        }
        
        if (input$inclocexport == TRUE){
          latfinal = input$latexport
          lonfinal = input$lonexport
        }else{
          latfinal = NA
          lonfinal = NA
        }
        
        #Date Time Fields
        if (input$datetypeexport == "Combined"){
          datetimecombined = input$datetimecombinecolexport
          dateonly = NA
          timeonly = NA
        }else if (input$datetypeexport == "Separate"){
          datetimecombined = NA
          dateonly = input$datecolexport
          timeonly = input$timecolexport
        }
        
        #Data Fields
        if (!is.na(loggerexportfinal$AirBP) & loggerexportfinal$AirBP != ""){
          airbpfinal = input$airbpexport
        }else{
          airbpfinal = NA
        }
        
        if (!is.na(loggerexportfinal$AirTemp) & loggerexportfinal$AirTemp != ""){
          airtempfinal = input$airtempexport
        }else{
          airtempfinal = NA
        }
        
        if (!is.na(loggerexportfinal$Chlorophylla) & loggerexportfinal$Chlorophylla != ""){
          chlorophyllafinal = input$chlorophyllaexport
        }else{
          chlorophyllafinal = NA
        }
        
        if (!is.na(loggerexportfinal$Cond) & loggerexportfinal$Cond != ""){
          condfinal = input$condexport
        }else{
          condfinal = NA
        }
        
        if (!is.na(loggerexportfinal$Discharge) & loggerexportfinal$Discharge != ""){
          dischargefinal = input$dischargeexport
        }else{
          dischargefinal = NA
        }
        
        if (!is.na(loggerexportfinal$DO) & loggerexportfinal$DO != ""){
          dofinal = input$doexport
        }else{
          dofinal = NA
        }
        
        if (!is.na(loggerexportfinal$pH) & loggerexportfinal$pH != ""){
          phfinal = input$phexport
        }else{
          phfinal = NA
        }
        
        if (!is.na(loggerexportfinal$Turbidity) & loggerexportfinal$Turbidity != ""){
          turbidityfinal = input$turbidityexport
        }else{
          turbidityfinal = NA
        }
        
        if (!is.na(loggerexportfinal$WaterLevel) & loggerexportfinal$WaterLevel != ""){
          waterlevelfinal = input$waterlevelexport
        }else{
          waterlevelfinal = NA
        }
        
        if (!is.na(loggerexportfinal$WaterP) & loggerexportfinal$WaterP != ""){
          waterpfinal = input$waterpexport
        }else{
          waterpfinal = NA
        }
        
        if (!is.na(loggerexportfinal$WaterTemp) & loggerexportfinal$WaterTemp != ""){
          watertempfinal = input$watertempexport
        }else{
          watertempfinal = NA
        }
        
        message(paste0("Model Name: ",input$modelnameexport,";\n Program Name: ",input$programnameexport,";\n Waterbody ID: ",input$wbidexport,
                       ";\n Waterbody Name: ",input$wbnameexport,";\n Waterbody Type: ",input$wbtypeexport,";\n Station ID: ",input$stationidexport,
                       ";\n Station Name: ",input$stationnameexport,";\n Deployment Count: ",input$deploymentexport,";\n User Name: ",input$usernameexport,
                       ";\n Z Value: ",input$zexport,";\n Latitude: ",input$latexport,";\n Longitude: ",input$lonexport,";"))
        
        exportfinalrow = data.frame("ProgramID" = input$selectedprogramexport,"ModelID" = input$selectloggerexport,"FileSep" = input$sepfile,
                                    "IncMeta" = input$incmeta,"IncRep" = input$increp,"IncConfig" = input$incconfig,
                                    "IncSum" = input$incsummary,"UnitID" = unitidfinal,"IncModelName" = input$incmodelnameexport,
                                    "ModelName" = modelnamefinal,"IncProgramName" = input$incprogramnameexport,
                                    "ProgramName" = programnamefinal,"IncProgramWBID" = input$incwbidexport,"ProgramWBID" = wbidfinal,
                                    "IncWBName" = input$incwbnameexport,"WBName" = wbnamefinal,"IncWBType" = input$incwbtypeexport,
                                    "WBType" = wbtypefinal,"IncProgramStationID" = input$incstationidexport,
                                    "ProgramStationID" = stationidfinal,"IncStationName" = input$incstationnameexport,
                                    "StationName" = stationnamefinal,"IncDeploy" = input$incdeploymentexport,"Deployment" = deploymentfinal,
                                    "DateTimeSep" = input$datetypeexport,"Date_Time" = datetimecombined,"Date" = dateonly,"Time" = timeonly,
                                    "TZ" = input$tzexport,"IncZ" = input$inczexport,"Z" = zfinal,"IncLoc" = input$inclocexport,
                                    "Lat" = latfinal,"Lon" = lonfinal,"IncUser" = input$incusernameexport,"User" = usernamefinal,
                                    "AirBP" = airbpfinal,"AirTemp" = airtempfinal,"Chlorophylla" = chlorophyllafinal,"Cond" = condfinal,
                                    "Discharge" = dischargefinal,"DO" = dofinal,"pH" = phfinal,
                                    "Turbidity" = turbidityfinal,"WaterLevel" = waterlevelfinal,"WaterP" = waterpfinal,"WaterTemp" = watertempfinal,
                                    "IncNotes" = input$incnotes,
                                    stringsAsFactors = FALSE)
        
        exportfinal = rbind(exportfinal,exportfinalrow)
      }
      export(exportfinal)
      updatebaseconfig()
    }else{
      message("Field Names not provided for every selected field")
      
      sendSweetAlert(
        session = session,
        title = "Missing Field Names",
        text = "Ensure a Field Name has been supplied for every checked box.",
        type = "error"
      )
    }
  }
)

#UI for descriptions
output$exportoptionsdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Customize which data the app will export and how the data will be organized.",
    tags$br(),
    tags$br(),
    "If a logger model collects multiple metrics, they can all be combined into a single file or each metric can be exported into 
    separate files.",
    tags$br(),
    tags$br(),
    "Select which fields should included in the data file and what those field names should be. Note that the Unit ID Field is required.",
    tags$br(),
    tags$br(),
    "The Date and Time fields can either be combined or separated and the relevant time zone needs to be selected.",
    HTML("</i></font>")
  )
})