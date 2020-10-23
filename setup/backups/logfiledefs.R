#Add Edit Delete Main UI
output$aedoptionsUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 12,
        radioGroupButtons(
          inputId = "aedmodels",
          choices = c("Add","Edit","Delete"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok",lib = "glyphicon"))
        )
      )    
    ),
    uiOutput("aedoptionschoiceUI")
  )
})

#Add Edit Delete 
output$aedoptionschoiceUI = renderUI({
  
  loggerpresets = loggerfiledefs()
  
  if (input$aedmodels == "Add"){
    tagList(
      fluidRow(
        column(
          width = 3,
          # tags$br(),
          textInput(
            inputId = "addloggermodelnametxt",
            label = NULL,
            placeholder = "Logger Model Name"
          )
        ),
        column(
          width = 2,
          HTML("<p align=right><b>Logger Types:</b></p>")
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "modelloggertypesadd",
            label = NULL,
            choices = loggerchoices,
            multiple = TRUE
          )
        )
      ),
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
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "lfeditloggermodelchoices",
          label = NULL,
          choices = loggermodelsedit()
          
        )
      ),
      column(
        width = 3,
        uiOutput("lfeditloggermodelnameUI")
      ),
      column(
        width = 2,
        HTML("<p align=right><b>Logger Types:</b></p>")
      ),
      column(
        width = 3,
        uiOutput("editloggermodeltypesUI")
      )
    ),
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
        width = 4,
        pickerInput(
          inputId = "deleteloggermodelchoices",
          label = NULL,
          choices = loggermodelsedit()
        )
      ),
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
          tagList(
            if ("AirBP" %in% input$modelloggertypesadd){
              textInput(
                inputId = "airbpfieldadd",
                label = "Air Pressure Field"
              )
              
            },
            if ("AirTemp" %in% input$modelloggertypesadd){
              textInput(
                inputId = "airtempfieldadd",
                label = "Air Temperature Field"
              )
              
            },
            if ("Chlorophylla" %in% input$modelloggertypesadd){

              textInput(
                inputId = "chlorophyllafieldadd",
                label = "Chlorophyll A Field"
              )
            },
            if ("Cond" %in% input$modelloggertypesadd){
    
              textInput(
                inputId = "condfieldadd",
                label = "Conductivity Field"
              )
            },
            if ("Discharge" %in% input$modelloggertypesadd){

              textInput(
                inputId = "dischargefieldadd",
                label = "Discharge Field"
              )
            },
            if ("DO" %in% input$modelloggertypesadd){

              textInput(
                inputId = "dofieldadd",
                label = "Dissolved Oxygen Field"
              )
            },
            if ("GageHeight" %in% input$modelloggertypesadd){

              textInput(
                inputId = "gageheightfieldadd",
                label = "Gage Height Field"
              )
            },
            if ("pH" %in% input$modelloggertypesadd){

              textInput(
                inputId = "phfieldadd",
                label = "pH Field"
              )
            },
            if ("Turbidity" %in% input$modelloggertypesadd){
    
              textInput(
                inputId = "turbidityfieldadd",
                label = "Turbidity Field"
              )
            },
            if ("WaterP" %in% input$modelloggertypesadd){

              textInput(
                inputId = "waterpfieldadd",
                label = "Water Pressure Field"
              )
            },
            if ("WaterTemp" %in% input$modelloggertypesadd){

              textInput(
                inputId = "watertempfieldadd",
                label = "Water Temperature Field"
              )
            }
          )
        )
      )
    )
  )
})

#Add Logger Buttons----
#Add New Logger Model
observeEvent(
  input$saveloggerfieldsbttn,
  {
    if (nchar(input$addloggermodelnametxt) > 0){
      print(1)
      if (length(input$modelloggertypesadd) > 0){
        print(2)
        newloggerdefs=loggerfiledefs()
        
        if (!is.null(input$datetimecombinecoladd)){
          print(3)
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
        
        if (all(c(!is.na(input$airbpfieldadd),!is.na(input$airtempfieldadd),!is.na(input$chlorophyllafieldadd),!is.na(input$condfieldadd),
                  !is.na(input$dischargefieldadd),!is.na(input$dofieldadd),!is.na(input$gageheightfieldadd),!is.na(input$phfieldadd),
                  !is.na(input$turbidityfieldadd),!is.na(input$waterpfieldadd),!is.na(input$watertempfieldadd)))){
          print(4)
          if (nchar(input$airbpfieldadd)>0){
            airbpentry = input$airbpfieldadd
            print(airbpentry)
            updatetexInput(
              session = session,
              inputId = "airbpfieldadd",
              value = NA
            )
          }else{
            airbpentry = NA
          }
          
          if (nchar(input$airtempfieldadd)>0){
            airtempentry = input$airtempfieldadd
            print(airtempentry)
            updateTextInput(
              session = session,
              inputId = "airtempfieldadd",
              value = NA
            )
          }else{
            airtempentry = NA
          }
          
          if (nchar(input$chlorophyllafieldadd)>0){
            chlorophyllaentry = input$chlorophyllafieldadd
            print(chlorophyllaentry)
            updateTextInput(
              session = session,
              inputId = "chlorophyllafieldadd",
              value = NA
            )
          }else{
            chlorophyllaentry = NA
          }
          
          if (nchar(input$condfieldadd)>0){
            condentry = input$condfieldadd
            print(condentry)
            updateTextInput(
              session = session,
              inputId = "condfieldadd",
              value = NA
            )
          }else{
            condentry = NA
          }
          
          if (nchar(input$dischargefieldadd)>0){
            dischargeentry = input$dischargefieldadd
            print(dischargeentry)
            updateTextInput(
              session = session,
              inputId = "dischargefieldadd",
              value = NA
            )
          }else{
            dischargeentry = NA
          }
          
          if (nchar(input$dofieldadd)>0){
            doentry = input$dofieldadd
            print(doentry)
            updateTextInput(
              session = session,
              inputId = "dofieldadd",
              value = NA
            )
          }else{
            doentry = NA
          }
          
          if (nchar(input$gageheightfieldadd)>0){
            gageheightentry = input$gageheightfieldadd
            print(gageheightentry)
            updateTextInput(
              session = session,
              inputId = "gageheightfieldadd",
              value = NA
            )
          }else{
            gageheightentry = NA
          }
          
          if (nchar(input$phfieldadd)>0){
            phentry = input$phfieldadd
            print(phentry)
            updateTextInput(
              session = session,
              inputId = "phfieldadd",
              value = NA
            )
          }else{
            phentry = NA
          }
          
          if (nchar(input$turbidityfieldadd)>0){
            turbidityentry = input$turbidityfieldadd
            print(turbidityentry)
            updateTextInput(
              session = session,
              inputId = "turbidityfieldadd",
              value = NA
            )
          }else{
            turbidityentry = NA
          }
          
          if (nchar(input$waterpfieldadd)>0){
            waterpentry = input$waterpfieldadd
            print(waterpentry)
            updateTextInput(
              session = session,
              inputId = "waterpfieldadd",
              value = NA
            )
          }else{
            waterpentry = NA
          }
          
          if (nchar(input$watertempfieldadd)>0){
            watertempentry = input$watertempfieldadd
            print(watertempentry)
            updateTextInput(
              session = session,
              inputId = "watertempfieldadd",
              value = NA
            )
          }else{
            watertempentry = NA
          }
          
          newloggerdf=data.frame("Logger_Model" = input$addloggermodelnametxt,"ModelID" = random_id(1,6),"Date" = dateentry,"Time" = timeentry,
                                 "DateTime" = datetimeentry,"DateFormat" = input$dateformatadd,"TimeFormat" = input$timeformatadd,"AirBP" = airbpentry,
                                 "AirTemp" = airtempentry,"Chlorophylla" = chlorophyllaentry,"Cond" = condentry,"Discharge" = dischargeentry,
                                 "DO" = doentry,"GageHeight" = gageheightentry,"pH" = phentry,"Turbidity" = turbidityentry,"WaterP" = waterpentry,
                                 "WaterTemp" = watertempentry,"TZ" = input$tzadd,"FieldNamesRow" = input$fieldnamerownumadd,
                                 "DataStartRow" = input$datarownumadd)
          
          newloggerdefs=rbind(newloggerdefs,newloggerdf)
          
          loggerfiledefs(newloggerdefs)
          updatebaseconfig()
          
        }else{
          sendSweetAlert(
            session = session,
            title = "Missing Data Fields",
            text = "Please select and enter in data field names",
            type = "warning"
          )
        }
      }
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

#Logger Model Logger Types
loggermodeltypeselectedit = reactive({
  loggermodeltypes = loggermodelselectedit()
  loggermodeltypes = names(Filter(function(x)!all(is.na(x)),loggermodeltypes[,c(8:18)]))
  return(loggermodeltypes)
})


#Edit Logger Model Name Text UI
output$lfeditloggermodelnameUI = renderUI({
  textInput(
    inputId = "editloggermodelname",
    value = names(loggermodelsedit()[loggermodelsedit() == input$lfeditloggermodelchoices]),
    label = NULL
  )
})

#Edit Logger Types UI
output$editloggermodeltypesUI = renderUI({
  pickerInput(
    inputId = "modelloggertypesedit",
    label = NULL,
    choices = loggerchoices,
    multiple = TRUE,
    selected = loggermodeltypeselectedit()
  )
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
          tagList(
            if ("AirBP" %in% input$modelloggertypesedit){
              
              textInput(
                inputId = "airpfieldedit",
                label = "Air Pressure Field",
                value = datafieldsedit$AirBP
              )
              
            },
            if ("AirTemp" %in% input$modelloggertypesedit){
              
              textInput(
                inputId = "airtempfieldedit",
                label = "Air Temperature Field",
                value = datafieldsedit$AirTemp
              )
              
            },
            if ("Chlorophylla" %in% input$modelloggertypesedit){
              textInput(
                inputId = "chlorophyllafieldedit",
                label = "Chlorophyll A Field",
                value = datafieldsedit$Chlorophylla
              )
            },
            if ("Cond" %in% input$modelloggertypesedit){
              textInput(
                inputId = "condfieldedit",
                label = "Conductivity Field",
                value = datafieldsedit$Cond
              )
            },
            if ("Discharge" %in% input$modelloggertypesedit){
              textInput(
                inputId = "dischargefieldedit",
                label = "Discharge Field",
                value = datafieldsedit$Discharge
              )
            },
            if ("DO" %in% input$modelloggertypesedit){
              textInput(
                inputId = "dofieldedit",
                label = "Dissolved Oxygen Field",
                value = datafieldsedit$DO
              )
            },
            if ("GageHeight" %in% input$modelloggertypesedit){
              textInput(
                inputId = "gageheightfieldedit",
                label = "Gage Height Field",
                value = datafieldsedit$GageHeight
              )
            },
            if ("pH" %in% input$modelloggertypesedit){
              textInput(
                inputId = "phfieldedit",
                label = "pH Field",
                value = datafieldsedit$pH
              )
            },
            if ("Turbidity" %in% input$modelloggertypesedit){
              textInput(
                inputId = "turbidityfieldedit",
                label = "Turbidity Field",
                value = datafieldsedit$Turbidity
              )
            },
            if ("WaterP" %in% input$modelloggertypesedit){
              textInput(
                inputId = "waterpfieldedit",
                label = "Water Pressure Field",
                value = datafieldsedit$WaterP
              )
            },
            if ("WaterTemp" %in% input$modelloggertypesedit){
              textInput(
                inputId = "watertempfieldedit",
                label = "Water Temperature Field",
                value = datafieldsedit$WaterTemp
              )
            }
          )
        )
      )
    )
  )
})

#Edit Button----
#Edit Model Name
observeEvent(
  input$editloggerfieldsbttn,
  {
    editloggerdefs = loggerfiledefs()
    editloggerdefs$Logger_Model[which(editloggerdefs$ModelID == input$editloggermodelchoices)] = input$editloggermodelname
    
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
    "Add new logger models, edit the name of existing models, or delete models.",
    HTML("</i></font>")
  )
})

output$lfloggerconfigdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Define the data fields that are present in the selected logger model data. This app requires that the input data is in the .csv format and that the field names are on the 
    top row of the data table.",
    HTML("</i></font>")
  )
})
