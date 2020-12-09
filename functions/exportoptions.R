#Main Export UI
output$exportoptionsUI = renderUI({
  
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
      width = 3
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

#Export Settings UI
output$exportsettingsUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 3,
        tags$div(
          style="vertical-align:center; border:1px solid lightgray; padding-right:10px; padding-left:10px; background-color:ghostwhite; width: 100%",
          tags$h4("Files"),
          awesomeRadio(
            inputId = "sepfile",
            label = NULL,
            choices = c("Single","Multiple"),
            selected = "Single",
            status = "success"
          )
        )
      ),
      column(
        width = 9,
        tags$div(
          style="vertical-align:center; border:1px solid lightgray; padding-right:10px; padding-left:10px; background-color:ghostwhite; width: 100%",
          tags$h4("Include Support Files"),
          fluidRow(
            column(
              width = 4,
              prettyCheckbox(
                inputId = "incmeta",
                label = "Metadata",
                value = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              prettyCheckbox(
                inputId = "increp",
                label = "Report",
                value = TRUE,
                status = "success"
              )
            ),
            column(
              width = 4,
              prettyCheckbox(
                inputId = "incconfig",
                label = "Config",
                value = TRUE,
                status = "success"
              )
            )
          )
        )
      )
    ),
    tags$br(),
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
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
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
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
              uiOutput("datetimeexportUI")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 9,
          uiOutput("timesettingsexportUI")
        )
      )
    ),
    tags$br(),
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
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
    )
  )
})

namesdeployexportselect = reactive({
  fluidRow(
    column(
      width = 2,
      
    ),
    column(
      width = 10
    )
  )
  
})


namesdeployfields = reactive({
  namesdeploy = export()
  namesdeploy = namesdeploy[which(namesdeploy$ProgramID == input$selectedprogramexport & namesdeploy$ModelID == input$selectloggerexport),]
  return(namesdeploy)
})


#Waterbody Names and Deployment Information UI
output$namesdeployexportUI = renderUI({
  namesdeployfieldvalues = namesdeployfields()
  tagList(
    fluidRow(
      column(
        width = 4,
        tags$br(),

        prettyCheckbox(
          inputId = "incwbidexport",
          label = "Waterbody ID",
          value = namesdeployfieldvalues$IncProgramWBID,
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
          value = namesdeployfieldvalues$IncWBName,
          status = "success"
        )
      ),
      column(
        width = 8,
        uiOutput("wbnameexportUI")
      )
    )
  )
})

#WBID Field Name UI
output$wbidexportUI = renderUI({
  incwbidexportvalue = namesdeployfields()
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
  incwbnameexportvalue = namesdeployfields()
  if (input$incwbnameexport == TRUE){
    textInput(
      inputId = "wbnameexport",
      label = "Waterbody Name Field Name",
      value = incwbnameexportvalue$WBName
    )
  }else{}
})

#Add Logger Date Fields
output$datefieldexportUI = renderUI({
  radioGroupButtons(
    inputId ="datetypeexport",
    label = "Field Organization",
    choices = c("Combined","Separate"),
    status = "primary"
  )
})

#Date Time Fields Main UI
output$datetimeexportUI = renderUI({
  validate(
    need(input$datetypeexport,"Loading...")
  )
  if (input$datetypeexport == "Combined"){
    fluidRow(
      column(
        width = 10,
        textInput(
          inputId = "datetimecombinecolexport",
          label = "Date Time Field Name"
        )
      )
    )
  }else{
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "datecolexport",
          label = "Date Field Name"
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = "timecolexport",
          label = "Time Field Name"
        )
      )
    )
  }
})

#Date Time UI
output$timesettingsexportUI = renderUI({
  
  fluidRow(
    column(
      width = 8,
      uiOutput("datetimeformatexportUI")
    ),
    column(
      width = 4,
      pickerInput(
        inputId = "tzexport",
        label = "Time Zone",
        choices = OlsonNames()
      )
    )
  )
})

#Date Time Format UI
output$datetimeformatexportUI = renderUI({
  
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = "dateformatexport",
        label = "Date Format",
        #dateformats located in the logfiledefs.R file
        choices = dateformats
      )
    ),
    column(
      width = 6,
      pickerInput(
        inputId = "timeformatexport",
        label = "Time Format",
        #timeformats located in the logfiledefs.R file
        choices = timeformats
      )
    )
  )
})

#Select Logger Types in model
modelnamesselectexport = reactive({
  modelexport = loggerfiledefs()

  modelexport = modelexport[which(modelexport$ModelID == input$selectloggerexport),]
  modelexport = modelexport[,8:18]
  
  modelexportnona = modelexport %>% select_if(function(x){all(!is.na(x))})
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
    print("DO")
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"DO" = exportnames$DO)
    }else{
      exportnameslist = c(exportnameslist,"DO" = NA)
    }
  }
  
  if ("GageHeight" %in% modelnamesselectexport()){
    if (nrow(exportnames) > 0){
      exportnameslist = c(exportnameslist,"GageHeight" = exportnames$GageHeight)
    }else{
      exportnameslist = c(exportnameslist,"GageHeight" = NA)
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
  
  print(exportnameslist)
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
        label = "AirBP Field Name",
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
    if ("GageHeight" %in% names(exportfieldnames)){
      textInput(
        inputId = "gageheightexport",
        label = "GageHeight Field Name",
        value = exportfieldnames["GageHeight"]
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