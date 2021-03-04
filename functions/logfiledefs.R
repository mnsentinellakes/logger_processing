#Add Edit Delete Main UI
output$aedoptionsUI = renderUI({
  tagList(
    tags$div(
      style="border:1px solid lightgray; padding:10px; background-color:ghostwhite;",
      fluidRow(
        column(
          width = 3,
          radioGroupButtons(
            inputId = "aedmodels",
            choices = c("Add","Edit","Delete"),
            status = "primary",
            checkIcon = list(
              yes = icon("ok",lib = "glyphicon"))
          )
        ),
        column(
          width = 9,
          uiOutput("aedmodelnamesUI")
        )
      )
    ),
    tags$br(),
    uiOutput("aedoptionschoiceUI")
  )
})

#Add Edit Delete Model Names
output$aedmodelnamesUI = renderUI({
  if (input$aedmodels == "Add"){
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "addloggermodelnametxt",
          label = NULL,
          placeholder = "Logger Model Name"
        )
      )
    )
  }else if (input$aedmodels == "Edit"){
    fluidRow(
      column(
        width = 6,
        isolate(
        pickerInput(
          inputId = "lfeditloggermodelchoices",
          label = NULL,
          choices = loggermodelsedit()
        )
        )
      ),
      column(
        width = 6,
        uiOutput("lfeditloggermodelnameUI")
      )
    )
  }else if (input$aedmodels == "Delete"){
    fluidRow(
      column(
        width = 6,
        pickerInput(
          inputId = "deleteloggermodelchoices",
          label = NULL,
          choices = loggermodelsedit()
        )
      )
    )
  }
})

output$lfeditloggermodelnameUI = renderUI({
  textInput(
    inputId = "editloggermodelname",
    value = names(loggermodelsedit()[loggermodelsedit() == input$lfeditloggermodelchoices]),
    label = NULL
  )
})

#Add Edit Delete 
output$aedoptionschoiceUI = renderUI({
  
  # loggerpresets = loggerfiledefs()
  
  if (input$aedmodels == "Add"){
    tagList(
      uiOutput("lfaddmodeltableconfigUI"),
      tags$br(),
      uiOutput("lfaddmodeltimeconfigUI"),
      tags$br(),
      uiOutput("lfaddmodeldataconfigUI"),
      tags$br(),
      actionBttn(
        inputId = "saveloggerfieldsbttn",
        label = "Save Logger Model Settings",
        style = "fill",
        color = "success"
      )
    )
  }else if (input$aedmodels == "Edit"){
    tagList(
      uiOutput("lfeditmodeltableconfigUI"),
      tags$br(),
      uiOutput("lfeditmodeltimeconfigUI"),
      tags$br(),
      uiOutput("lfeditmodeldataconfigUI"),
      tags$br(),
      actionBttn(
        inputId = "editloggerfieldsbttn",
        label = "Edit Logger Model Settings",
        style = "fill",
        color = "warning"
      )
    )
  }else if (input$aedmodels == "Delete"){
    fluidRow(
      column(
        width = 2,
        actionBttn(
          inputId = "deleteloggermodelnamebttn",
          label = "Delete",
          style = "fill",
          color = "danger"
        )
      )
    )
  }
})

#Add Loggers-----

#Add Logger Table Configuration----
#Table Row Config
output$lfaddmodeltableconfigUI = renderUI({
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      
      fluidRow(
        column(
          width = 2,
          tags$h4("Row Selection")
        ),
        column(
          width = 3,
          numericInput(
            inputId = "fieldnamerownumadd",
            label = "Row Number with Field Names",
            value = 1,
            step = 1,
            width = 120
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "datarownumadd",
            label = "First Row Number with Data",
            value = 2,
            step = 1,
            width = 120
          )
        )
      )
    )
  )
})

#Add Logger Date and Time-----
#Define Date and Time Formats
dateformats = c("%m/%d/%Y","%m/%d/%y","%b/%d/%Y","%b/%d/%y","%Y-%m-%d","%y-%m-%d")
timeformats = c("%H:%M","%H:%M:%S","%I:%M %p","%I:%M:%S %p")

#Date and Time Fields Config
output$lfaddmodeltimeconfigUI = renderUI({
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 2,
          tags$h4("Date and Time Fields")
        ),
        column(
          width = 10,
          fluidRow(
            column(
              width = 3,
              uiOutput("lfadddatefieldUI")
            ),
            column(
              width = 4,
              uiOutput("lfadddatetimeUI")
            )
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("lfaddtimesettingsUI")
            )
          )
        )
      )
    )
  )
})

#Add Logger Date Fields
output$lfadddatefieldUI = renderUI({
  radioGroupButtons(
    inputId ="datetypeadd",
    label = "Field Organization",
    choices = c("Combined","Separate"),
    status = "primary"
  )
})

#Add Date Time Fields Main UI
output$lfadddatetimeUI = renderUI({
  validate(
    need(input$datetypeadd,"Loading...")
  )
  if (input$datetypeadd == "Combined"){
    fluidRow(
      column(
        width = 7,
        textInput(
          inputId = "datetimecombinecoladd",
          label = "Date Time Field Name"
        )
      )
    )
  }else{
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "datecoladd",
          label = "Date Field Name"
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = "timecoladd",
          label = "Time Field Name"
        )
      )
    )
  }
})

#Date Time Format UI
output$lfadddatetimeformatUI = renderUI({
  
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = "dateformatadd",
        label = "Date Format",
        choices = dateformats
      )
    ),
    column(
      width = 6,
      pickerInput(
        inputId = "timeformatadd",
        label = "Time Format",
        choices = timeformats
      )
    )
  )
})

#Date Time UI
output$lfaddtimesettingsUI = renderUI({
  
  fluidRow(
    column(
      width = 6,
      uiOutput("lfadddatetimeformatUI")
    ),
    column(
      width = 3,
      pickerInput(
        inputId = "tzadd",
        label = "Time Zone",
        choices = OlsonNames()
      )
    )
  )
})

#Add Model Data Fields----
output$lfaddmodeldataconfigUI = renderUI({
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 2,
          tags$h4("Logger Data Fields")
        ),
        column(
          width = 5,
          textInput(
            inputId = "airbpfieldadd",
            label = "Air Pressure Field",
            value = NA
          ),
          textInput(
            inputId = "airtempfieldadd",
            label = "Air Temperature Field",
            value = NA
          ),
          textInput(
            inputId = "chlorophyllafieldadd",
            label = "Chlorophyll A Field",
            value = NA
          ),
          textInput(
            inputId = "condfieldadd",
            label = "Conductivity Field",
            value = NA
          ),
          textInput(
            inputId = "dischargefieldadd",
            label = "Discharge Field",
            value = NA
          ),
          textInput(
            inputId = "dofieldadd",
            label = "Dissolved Oxygen Field",
            value = NA
          )
        ),
        column(
          width = 5,
          textInput(
            inputId = "gageheightfieldadd",
            label = "Gage Height Field",
            value = NA
          ),
          textInput(
            inputId = "phfieldadd",
            label = "pH Field",
            value = NA
          ),
          textInput(
            inputId = "turbidityfieldadd",
            label = "Turbidity Field",
            value = NA
          ),
          textInput(
            inputId = "waterpfieldadd",
            label = "Water Pressure Field",
            value = NA
          ),
          textInput(
            inputId = "watertempfieldadd",
            label = "Water Temperature Field",
            value = NA
          )
        )
      )
    )
  )
})

#Add Logger Buttons----
#Add New Logger Model
observeEvent(
  input$saveloggerfieldsbttn,{
    if (nchar(input$addloggermodelnametxt) > 0){
      newloggerdefs = loggerfiledefs()
      if (!is.null(input$datetimecombinecoladd)){
        datetimeentry = input$datetimecombinecoladd
        dateentry = NA
        timeentry = NA
        
        updateTextInput(
          session = session,
          inputId = "datetimecombinecoladd",
          value = NA
        )
      }else if (!is.null(input$datecoladd) & !is.null(input$timecoladd)){
        datetimeentry = NA
        dateentry = input$datecoladd
        timeentry = input$timecoladd
        
        updateTextInput(
          session = session,
          inputId = "datecoladd",
          value = NA
        )
        
        updateTextInput(
          session = session,
          inputId = "timecoladd",
          value = NA
        )
      }else{
        sendSweetAlert(
          session = session,
          title = "Missing Date and Time Fields",
          text = "Please enter in the names of the combined or separate date and time fields",
          type = "warning"
        )
      }
      
      generatemodelid = random_id(1,6)
      
      newloggerdf=data.frame("Logger_Model" = input$addloggermodelnametxt,"ModelID" = generatemodelid,"Date" = dateentry,"Time" = timeentry,
                             "DateTime" = datetimeentry,"Date_Format" = input$dateformatadd,"Time_Format" = input$timeformatadd,
                             "AirBP" = input$airbpfieldadd,"AirTemp" = input$airtempfieldadd,"Chlorophylla" = input$chlorophyllafieldadd,
                             "Cond" = input$condfieldadd,"Discharge" = input$dischargefieldadd,"DO" = input$dofieldadd,
                             "GageHeight" = input$gageheightfieldadd,"pH" = input$phfieldadd,"Turbidity" = input$turbidityfieldadd,
                             "WaterP" = input$waterpfieldadd,"WaterTemp" = input$watertempfieldadd,"TZ" = input$tzadd,
                             "FieldNamesRow" = input$fieldnamerownumadd,"DataStartRow" = input$datarownumadd)
      
      newloggerdefs=rbind(newloggerdefs,newloggerdf)
      
      loggerfiledefs(newloggerdefs)
      updatebaseconfig()
      updateTextInput(
        session = session,
        inputId = "addloggermodelnametxt",
        value = NA
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "Missing Model Name",
        text = "Please enter in a model name",
        type = "warning"
      )
    }
  }
)

#Edit Loggers-----
#Edit Logger Names and Types----
#Previously Created Logger models
loggermodelsedit = reactive({
  loggermodelselect = loggerfiledefs()
  loggermodelselectids = loggermodelselect$ModelID
  names(loggermodelselectids) = loggermodelselect$Logger_Model
  
  return(loggermodelselectids)
})

#Select Logger Model Data
loggermodelselectedit = reactive({
  loggermodelrows = loggerfiledefs()
  loggermodelrows = loggermodelrows[which(loggermodelrows$ModelID == input$lfeditloggermodelchoices),]
  return(loggermodelrows)
})

#Edit Logger Table Configuration----
output$lfeditmodeltableconfigUI = renderUI({
  loggermodelselectedit = loggermodelselectedit()
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      
      fluidRow(
        column(
          width = 2,
          tags$h4("Row Selection")
        ),
        column(
          width = 3,
          numericInput(
            inputId = "fieldnamerownumedit",
            label = "Row Number with Field Names",
            value = loggermodelselectedit$FieldNamesRow,
            step = 1,
            width = 120
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "datarownumedit",
            label = "First Row Number with Data",
            value = loggermodelselectedit$DataStartRow,
            step = 1,
            width = 120
          )
        )
      )
    )
  )
})


#Edit Logger Date and Time Fields----
#Date and Time Fields Config
output$lfeditmodeltimeconfigUI = renderUI({
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 2,
          tags$h4("Date and Time Fields")
        ),
        column(
          width = 10,
          fluidRow(
            column(
              width = 3,
              uiOutput("lfeditdatefieldUI")
            ),
            column(
              width = 4,
              uiOutput("lfeditdatetimeUI")
            )
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("lfedittimesettingsUI")
            )
          )
        )
      )
    )
  )
})

#Edit Logger Date Fields
output$lfeditdatefieldUI = renderUI({
  validate(
    need(loggermodelselectedit,("Loading..."))
  )
  
  datetypeeditselect = loggermodelselectedit()
  
  if(!is.na(datetypeeditselect$Date)){
    selectedchoice = "Seperate"
  }else{
    selectedchoice = "Combined"
  }
  
  radioGroupButtons(
    inputId ="datetypeedit",
    label = "Field Organization",
    choices = c("Combined","Separate"),
    status = "primary",
    selected = selectedchoice
  )
})

#Edit Date Time Fields Main UI
output$lfeditdatetimeUI = renderUI({
  validate(
    need(input$datetypeedit,"Loading...")
  )
  
  datetimetypeselect = loggermodelselectedit()
  
  if (input$datetypeedit == "Combined"){
    fluidRow(
      column(
        width = 8,
        textInput(
          inputId = "datetimecombinecoledit",
          label = "Date Time Field Name",
          value = datetimetypeselect$DateTime
        )
      )
    )
  }else{
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "datecoledit",
          label = "Date Field Name",
          value = datetimetypeselect$Date
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = "timecoledit",
          label = "Time Field Name",
          value = datetimetypeselect$Time
        )
      )
    )
  }
})

#Date Time UI
output$lfedittimesettingsUI = renderUI({
  
  timezoneselect = loggermodelselectedit()
  
  fluidRow(
    column(
      width = 6,
      uiOutput("lfeditdatetimeformatUI")
    ),
    column(
      width = 3,
      pickerInput(
        inputId = "tzedit",
        label = "Time Zone",
        choices = OlsonNames(),
        selected = timezoneselect$TZ
      )
    )
  )
})

#Date Time Format UI
output$lfeditdatetimeformatUI = renderUI({
  
  datetimeformatselect = loggermodelselectedit()
  
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = "dateformatedit",
        label = "Date Format",
        choices = dateformats,
        selected = datetimeformatselect$Date_Format
      )
    ),
    column(
      width = 6,
      pickerInput(
        inputId = "timeformatedit",
        label = "Time Format",
        choices = timeformats,
        selected = datetimeformatselect$Time_Format
      )
    )
  )
})

#Edit Model Data Fields----
output$lfeditmodeldataconfigUI = renderUI({
  
  datafieldsedit = loggermodelselectedit()
  tagList(
    tags$div(
      style="vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite; width: 100%",
      fluidRow(
        column(
          width = 2,
          tags$h4("Logger Data Fields")
        ),
        column(
          width = 5,
          
          textInput(
            inputId = "airbpfieldedit",
            label = "Air Pressure Field",
            value = datafieldsedit$AirBP
          ),
          textInput(
            inputId = "airtempfieldedit",
            label = "Air Temperature Field",
            value = datafieldsedit$AirTemp
          ),
          textInput(
            inputId = "chlorophyllafieldedit",
            label = "Chlorophyll A Field",
            value = datafieldsedit$Chlorophylla
          ),
          textInput(
            inputId = "condfieldedit",
            label = "Conductivity Field",
            value = datafieldsedit$Cond
          ),
          textInput(
            inputId = "dischargefieldedit",
            label = "Discharge Field",
            value = datafieldsedit$Discharge
          ),
          textInput(
            inputId = "dofieldedit",
            label = "Dissolved Oxygen Field",
            value = datafieldsedit$DO
          )
        ),
        column(
          width = 5,
          textInput(
            inputId = "gageheightfieldedit",
            label = "Gage Height Field",
            value = datafieldsedit$GageHeight
          ),
          textInput(
            inputId = "phfieldedit",
            label = "pH Field",
            value = datafieldsedit$pH
          ),
          textInput(
            inputId = "turbidityfieldedit",
            label = "Turbidity Field",
            value = datafieldsedit$Turbidity
          ),
          textInput(
            inputId = "waterpfieldedit",
            label = "Water Pressure Field",
            value = datafieldsedit$WaterP
          ),
          textInput(
            inputId = "watertempfieldedit",
            label = "Water Temperature Field",
            value = datafieldsedit$WaterTemp
          )
        )
      )
    )
  )
})

#Edit Button----
#Edit Model Name
observeEvent(
  input$editloggerfieldsbttn,{
    editloggerdefs = loggerfiledefs()
    
    #configure input data
    #Date column
    if (!is.null(input$datecoledit)){
      datecol = input$datecoledit
    }else{
      datecol = NA
    }
    
    #Time Column
    if(!is.null(input$timecoledit)){
      timecol = input$timecoledit
    }else{
      timecol = NA
    }
    
    #Date Time Column
    if(!is.null(input$datetimecombinecoledit)){
      datetimecombinecol = input$datetimecombinecoledit
    }else{
      datetimecombinecol = NA
    }
    
    #AirBP column
    if (!is.null(input$airbpfieldedit)){
      airbpfield = input$airbpfieldedit
    }else{
      airbpfield = NA
    }
    
    #AirTemp column
    if(!is.null(input$airtempfieldedit)){
      airtempfield = input$airtempfieldedit
    }else{
      airtempfield = NA
    }
    
    #Chlorophyll column
    if(!is.null(input$chlorophyllafieldedit)){
      chlorophyllafield = input$chlorophyllafieldedit
    }else{
      chlorophyllafield = NA
    }
    
    #Conductivity column
    if(!is.null(input$condfieldedit)){
      condfield = input$condfieldedit
    }else{
      condfield = NA
    }
    
    #Discharge column
    if(!is.null(input$dischargefieldedit)){
      dischargefield = input$dischargefieldedit
    }else{
      dischargefield = NA
    }
    
    #DO column
    if(!is.null(input$dofieldedit)){
      dofield = input$dofieldedit
    }else{
      dofield = NA
    }
    
    #Gageheight column
    if(!is.null(input$gageheightfieldedit)){
      gageheightfield = input$gageheightfieldedit
    }else{
      gageheightfield = NA
    }
    
    #pH column
    if(!is.null(input$phfieldedit)){
      phfield = input$phfieldedit
    }else{
      phfield = NA
    }
    
    #Turbidity column
    if(!is.null(input$turbidityfieldedit)){
      turbidityfield = input$turbidityfieldedit
    }else{
      turbidityfield = NA
    }
    
    #WaterP column
    if(!is.null(input$waterpfieldedit)){
      waterpfield = input$waterpfieldedit
    }else{
      waterpfield = NA
    }
    
    #WaterTemp column
    if(!is.null(input$watertempfieldedit)){
      watertempfield = input$watertempfieldedit
    }else{
      watertempfield = NA
    }
    
    editloggerdefs$Logger_Model[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$editloggermodelname
    editloggerdefs$Date[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = datecol
    editloggerdefs$Time[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = timecol
    editloggerdefs$DateTime[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = datetimecombinecol
    editloggerdefs$Date_Format[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$dateformatedit
    editloggerdefs$Time_Format[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$timeformatedit
    editloggerdefs$TZ[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$tzedit
    editloggerdefs$AirBP[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = airbpfield
    editloggerdefs$AirTemp[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = airtempfield
    editloggerdefs$Chlorophylla[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = chlorophyllafield
    editloggerdefs$Cond[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = condfield
    editloggerdefs$Discharge[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = dischargefield
    editloggerdefs$DO[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = dofield
    editloggerdefs$GageHeight[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = gageheightfield
    editloggerdefs$pH[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = phfield
    editloggerdefs$Turbidity[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = turbidityfield
    editloggerdefs$WaterP[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = waterpfield
    editloggerdefs$WaterTemp[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = watertempfield
    editloggerdefs$FieldNamesRow[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$fieldnamerownumedit
    editloggerdefs$DataStartRow[which(editloggerdefs$ModelID == input$lfeditloggermodelchoices)] = input$datarownumedit
    
    loggerfiledefs(editloggerdefs)
    updatebaseconfig()
    
  }
)

#Delete Button----
#Delete Model Name
observeEvent(
  input$deleteloggermodelnamebttn,
  {
    deleteloggerdefs = loggerfiledefs()
    deleteloggerdefs = deleteloggerdefs[which(deleteloggerdefs$ModelID !=input$deleteloggermodelchoices),]
    
    loggerfiledefs(deleteloggerdefs)
    updatebaseconfig()
  }
)

#Add Edit Delete Description
output$aedoptionsdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Add new logger models, edit existing models, or delete models.",
    HTML("</i></font>")
  )
})

output$lfloggerconfigdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>This section allows the user to define the data table organization for unique logger models. <br><br>In the 
    <b>Row Selection</b> box, select which rows contain the field names and the first row that contains the actual logger data. 
    <br><br>Define the Date and Time field organization, formats and fields in the <b>Date and Time Fields</b> box. <br><br>Copy the field 
    names of the relevant logger data types within the logger data table. Only the data fields with data need to be entered.</i></font>")
  )
})