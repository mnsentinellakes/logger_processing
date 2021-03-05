loadstatus = reactiveVal("")
`%notin%` <- Negate(`%in%`)

#Create a named vector of ProgramIDs and assign appropriate names to render selectprogramsUI
programid = reactive({
  programselect = programs()
  programselectvec = programselect$ProgramID
  names(programselectvec) = programselect$Program_Name
  return(programselectvec)
})

#Load Configuration File UI
output$loadconfigfileUI = renderUI({
  fluidRow(
    column(
      width = 6,
      fileInput(
        inputId = "baseconfigload",
        label = NULL,
        accept = ".RData",
        multiple = FALSE
      )
    ),
    column(
      width = 1,
      actionBttn(
        inputId = "baseconfigloadbttn",
        label = "Load",
        style = "fill",
        color = "success"
      )
    ),
    column(
      width = 4,
      HTML(paste0("<font size=4 color=gray> <p align = left>",loadstatus(),"</p></font>"))
    )
  )
})

#All code used to update potential changes to tables in baseconfig
updatebaseconfigversion = function(baseconfigdata){
  
  if("version" %notin% names(baseconfigdata)){
    baseconfigdata = c(baseconfigdata,"version" = 0.5)
  }else{
    baseconfigdata
  }
  return(baseconfigdata)
}

#Observe Config Load Button
observeEvent(
  input$baseconfigloadbttn,{
    if (!is.null(input$baseconfigload)){
      baseconfiginput = input$baseconfigload
      
      if (grepl(".RData",baseconfiginput$name,fixed = TRUE)){
        load(baseconfiginput$datapath)
        
        if (names(baseconfig)[1] == "programs"){
          
         baseconfig = updatebaseconfigversion(baseconfig)
         save(baseconfig,file = "config/baseconfig.RData")

         #Assign programs data frame to a reactive value
         programs(baseconfig$programs)
         #Assign program waterbodies data frame to a reactive value
         programwbs(baseconfig$programwbs)
         #Assign waterbody names data frame to a reactive value
         wbnames(baseconfig$wbnames)
         #Assign stations data frame to a reactive value
         stations(baseconfig$stations)
         #Assign processing logs data frame to a reactive value
         processinglogs(baseconfig$processinglogs)
         #Assign QC configuration settings data frame to a reactive value
         qc_config(baseconfig$qc_config)
         #Assign Logger File definitions to a reactive value
         loggerfiledefs(baseconfig$loggerfiledefs)
         #Assign Deploy Logs to a reactive value
         deploylogs(baseconfig$deploylogs)
         #Assign Export Settings to a reactive value
         export(baseconfig$export)
         
         # updatebaseconfig()
         loadstatus("Configuration File Loaded")
        }else{
          loadstatus("Incompatible File")
        }
      } else{
        loadstatus("Incorrect File Type")
      }
    }else{
      loadstatus("Uploaded Configuration File Missing")
    }
  }
)

#Load Configuration File Description UI
output$loadconfigfiledescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Upload previously created configuration files containing customized programs, waterbodies, qc settings, file settings, and tracking 
    information.",
    HTML("</i></font>")
  )
})

#Add Edit Delete Programs UI
output$aedprogramsUI = renderUI({
  tagList(
    radioGroupButtons(
      inputId = "aedprograms",
      choices = c("Add","Edit","Delete"),
      status = "primary",
      checkIcon = list(
        yes = icon("ok",lib = "glyphicon"))
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput("aedprogramselectchoiceUI")
      )
    )
  )
})

#Add Edit Delete Programs Description UI
output$aedprogramsdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Add new programs, edit the name of existing programs, or delete programs. Please note that if a program is deleted, all associated 
    waterbodies will be deleted as well.",
    HTML("</i></font>")
  )
})

#Add Edit Delete Program Choices UI
output$aedprogramselectchoiceUI = renderUI({
  
  if (input$aedprograms == "Add"){
    tagList(
      fluidRow(
        column(
          width = 7,
          textInput(
            inputId = "addprogramname",
            label = NULL,
            placeholder = "New Program Name"
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "addprogrambttn",
            label = "Add",
            style = "fill",
            color = "success"
          )
        )
      )
    )
  }else if (input$aedprograms == "Edit"){
    tagList(
      fluidRow(
        column(
          width = 5,
          pickerInput(
            inputId = "editprogramnamechoices",
            label = NULL,
            choices = programid()
          )
        ),
        column(
          width = 5,
          uiOutput("editprogramnameUI")
        ),
        column(
          width = 1,
          actionBttn(
            inputId = "editprogrambttn",
            label = "Edit",
            style = "fill",
            color = "warning"
          )
        )
      )
    )
  }else if (input$aedprograms == "Delete"){
    tagList(
      fluidRow(
        column(
          width = 7,
          pickerInput(
            inputId = "deleteprogramnamechoices",
            label = NULL,
            choices = programid()
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "deleteprogrambttn",
            label = "Delete",
            style = "fill",
            color = "danger"
          )
        )
      )
    )
  }
})

#Edit Program Name UI
output$editprogramnameUI = renderUI({
  textInput(
    inputId = "editprogramname",
    label = NULL,
    value = names(programid())[programid() == input$editprogramnamechoices]
  )
})

#Add Progam Event
observeEvent(
  input$addprogrambttn,{
    addprogramsrv = programs()
    
    programsaddrow = data.frame("ProgramID"=random_id(1,bytes = 6),"Program_Name"=input$addprogramname)
    addprogramsrvsave = rbind(addprogramsrv,programsaddrow)
    
    programs(addprogramsrvsave)
    updatebaseconfig()
    
    updateTextInput(
      session = session,
      inputId = "addprogramname",
      value = ""
    )
  }
)

#Edit Program Event
observeEvent(
  input$editprogrambttn,{
    editprogramsrv = programs() 
    editprogramsrv$Program_Name[which(editprogramsrv$ProgramID == input$editprogramnamechoices)] = input$editprogramname
    
    programs(editprogramsrv)
    updatebaseconfig()
    
  }
)

#Delete Program Event
observeEvent(
  input$deleteprogrambttn,{
    deleteprogramsrv = programs()
    deleteprogramwbsrv = programwbs()
    deleteprogramwbnamesrv = wbnames()
    deletestationsrv = stations()
    deleteprogramqcconfig = qc_config()
    
    appids = deleteprogramwbsrv$AppID[which(deleteprogramwbsrv$ProgramID == input$deleteprogramnamechoices)]
    
    deleteprogramsrv = deleteprogramsrv[which(deleteprogramsrv$ProgramID != input$deleteprogramnamechoices),]
    deleteprogramwbsrv = deleteprogramwbsrv[which(deleteprogramwbsrv$AppID %notin% appids),]
    deleteprogramwbnamesrv = deleteprogramwbnamesrv[which(deleteprogramwbnamesrv$AppID %notin% appids),]
    deletestationsrv = deletestationsrv[which(deletestationsrv$AppID %notin% appids),]
    deleteprogramqcconfig = deleteprogramqcconfig[which(deleteprogramqcconfig$AppID %notin% appids),]
    
    programs(deleteprogramsrv)
    programwbs(deleteprogramwbsrv)
    wbnames(deleteprogramwbnamesrv)
    stations(deletestationsrv)
    qc_config(deleteprogramqcconfig)
    
    updatebaseconfig()
    load("config/baseconfig.RData")
  }
)

#Add Edit Delete Waterbodies
output$aedwaterbodiesUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 6,
        radioGroupButtons(
          inputId = "aedwaterbodies",
          choices = c("Add","Edit","Delete"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok",lib = "glyphicon"))
        ),
        pickerInput(
          inputId = "waterbodyprogramselect",
          label = NULL,
          choices = programid()
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput("aedwaterbodiesselectchoiceUI")
      )
    )
  )
})

#Add Edit Delete Waterbodies description UI
output$aedwaterbodiesdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "For the selected program, add new waterbodies, edit the name and/or id of existing waterbodies, or delete waterbodies. Each waterbody 
    is required to be named. If it does not already have an existing id, one can be generated using the Generate Waterbody ID button. This can 
    also be left blank. Select the type of waterbody.",
    HTML("</i></font>")
  )
})

#Create a vector of AppIDS that are in the selected Program
programwbsselect = reactive({
  programwbschoice=programwbs()
  programwbschoice=programwbschoice$AppID[which(programwbschoice$ProgramID == input$waterbodyprogramselect)]
  return(programwbschoice)
})

#Create a vector of Waterbody Names from the AppIDS
programwbnamesselect = reactive({
  wbnamesselect=wbnames()
  wbnamesselect=wbnamesselect[which(wbnamesselect$AppID %in% programwbsselect()),]
  wbnamesselectvec=wbnamesselect$AppID
  names(wbnamesselectvec)=wbnamesselect$Waterbody_Name
  return(wbnamesselectvec)
})

#Waterbody Types
wbtypes = c("Lake","Reservoir","Stream","Wetland")

#Add Edit Delete Waterbody Choices UI
output$aedwaterbodiesselectchoiceUI = renderUI({
  if (input$aedwaterbodies == "Add"){
    tagList(
      fluidRow(
        column(
          width = 7,
          textInput(
            inputId = "addwaterbodiesname",
            label = NULL,
            placeholder = "New Waterbody Name"
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "addwaterbodiesbttn",
            label = "Add",
            style = "fill",
            color = "success"
          )
        )
      ),
      fluidRow(
        column(
          width = 7,
          textInput(
            inputId = "addwaterbodiesid",
            label = NULL,
            placeholder = "New Waterbody ID"
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "generatewbidbttn",
            label = "Generate Waterbody ID",
            style = "fill",
            color = "primary",
            size = "sm"
          )
        )
      ),
      fluidRow(
        column(
          width = 7,
          pickerInput(
            inputId = "addwaterbodiestype",
            choices = wbtypes,
            label = NULL
          )
        )
      )
    )
  }else if (input$aedwaterbodies == "Edit"){
    tagList(
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = "editwaterbodiesnamechoices",
            label = NULL,
            choices = programwbnamesselect()
          )
        )
      ),
      fluidRow(
        column(
          width = 7,
          uiOutput("editwaterbodiesnameUI")
        ),
        column(
          width = 1,
          tags$br(),
          tags$br(),
          actionBttn(
            inputId = "editwaterbodiesbttn",
            label = "Edit",
            style = "fill",
            color = "warning"
          )
        )
      )
    )
  }else if (input$aedwaterbodies == "Delete"){
    tagList(
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = "deletewaterbodiesnamechoices",
            label = NULL,
            choices = programwbnamesselect()
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "deletewaterbodiesbttn",
            label = "Delete",
            style = "fill",
            color = "danger"
          )
        )
      )
    )
  }
})

#Edit Waterbody Name and ID UI
output$editwaterbodiesnameUI = renderUI({
  waterbodyselect = programwbs()
  waterbodyids = waterbodyselect$ProgramWaterbodyID[which(waterbodyselect$AppID == input$editwaterbodiesnamechoices)]
  waterbodytypes = waterbodyselect$WB_Type[which(waterbodyselect$AppID == input$editwaterbodiesnamechoices)]
  tagList(
    textInput(
      inputId = "editwaterbodyname",
      label = NULL,
      value = names(programwbnamesselect()[programwbnamesselect() == input$editwaterbodiesnamechoices]),
      placeholder = "Add Waterbody Name"
    ),
    textInput(
      inputId = "editwaterbodyid",
      label = NULL,
      value = waterbodyids,
      placeholder = "Add Waterbody ID"
    ),
    pickerInput(
      inputId = "editwaterbodiestype",
      choices = wbtypes,
      selected = waterbodytypes,
      label = NULL
    )
  )
})

#Generate waterbody id Event
observeEvent(
  input$generatewbid,{
    updateTextInput(
      session = session,
      inputId = "addwaterbodiesid",
      value = random_id(1,4)
    )
  }
)

#Create a new waterbody in the program Event
observeEvent(
  input$addwaterbodiesbttn,{
    if (nchar(input$addwaterbodiesname) > 0){
      programwbsrv=programwbs()
      wbnamesrv=wbnames()
      newwbqcconfig=qc_config()
      defaultconfig=newwbqcconfig[which(newwbqcconfig$AppID == "1111111111111111"),]
      
      newappid = random_id(1,8)
      
      programwbsrvrow = data.frame("ProgramID"=input$waterbodyprogramselect,"ProgramWaterbodyID"=input$addwaterbodiesid,"AppID"=newappid,
                                   "WB_Type" = input$addwaterbodiestype)
      wbnamesrvrow = data.frame("AppID" = newappid,"Waterbody_Name" = input$addwaterbodiesname)
      
      programwbsrv = rbind(programwbsrv,programwbsrvrow)
      wbnamesrv = rbind(wbnamesrv,wbnamesrvrow)
      
      defaultconfig$AppID = newappid
      newwbqcconfig = rbind(newwbqcconfig,defaultconfig)
      
      programwbs(programwbsrv)
      wbnames(wbnamesrv)
      qc_config(newwbqcconfig)
      
      updatebaseconfig()
      
      updateTextInput(
        session = session,
        inputId = "addwaterbodiesname",
        value = ""
      )
      
      updateTextInput(
        session = session,
        inputId = "addwaterbodiesid",
        value = ""
      )
      
    }else if (nchar(input$addwaterbodiesname) <= 0){
      sendSweetAlert(
        session = session,
        title = "Missing Waterbody Name",
        text = "Please add a waterbody name.",
        type = "danger"
      )
    }
  }
)

#Edit existing waterbody name and id Event
observeEvent(
  input$editwaterbodiesbttn,{
    editprogramwbsrv = programwbs()
    editwbnamesrv = wbnames()
    editwbnamesrv$Waterbody_Name[which(editwbnamesrv$AppID == input$editwaterbodiesnamechoices)] = input$editwaterbodyname
    editprogramwbsrv$ProgramWaterbodyID[which(editprogramwbsrv$AppID == input$editwaterbodiesnamechoices)] = input$editwaterbodyid
    editprogramwbsrv$WB_Type[which(editprogramwbsrv$AppID == input$editwaterbodiesnamechoices)] = input$editwaterbodiestype
    programwbs(editprogramwbsrv)
    wbnames(editwbnamesrv)
    
    updatebaseconfig()
  }
)

#Delete waterbodies
observeEvent(
  input$deletewaterbodiesbttn,{
    deleteprogramwbsrv=programwbs()
    deletewbnamesrv=wbnames()
    deletestationsrv = stations()
    deleteqcconfig=qc_config()
    
    deleteprogramwbsrv=deleteprogramwbsrv[which(deleteprogramwbsrv$AppID != input$deletewaterbodiesnamechoices),]
    deletewbnamesrv=deletewbnamesrv[which(deletewbnamesrv$AppID != input$deletewaterbodiesnamechoices),]
    deletestationsrv = deletestationsrv[which(deletestationsrv$AppID != input$deletewaterbodiesnamechoices),]
    deleteqcconfig=deleteqcconfig[which(deleteqcconfig$AppID != input$deletewaterbodiesnamechoices),]
    
    programwbs(deleteprogramwbsrv)
    wbnames(deletewbnamesrv)
    stations(deletestationsrv)
    qc_config(deleteqcconfig)
    
    updatebaseconfig()
  }
)

output$aedstationsUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 12,
        radioGroupButtons(
          inputId = "aedstations",
          choices = c("Add","Edit","Delete"),
          status = "primary",
          checkIcon = list(
            yes = icon("ok",lib = "glyphicon"))
        ),
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = "stationprogramselect",
              label = NULL,
              choices = programid()
            )
          ),
          column(
            width = 6,
            uiOutput("stationwaterbodiesselectUI")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput("aedstationsselectchoiceUI")
      )
    )
  )
})

#Select program wbs for adding a station
stationwbs = reactive({
  programwbselect = programwbs()
  programwbselect = programwbselect$AppID[which(programwbselect$ProgramID == input$stationprogramselect)]
  wbnamesselect=wbnames()
  wbnamesselect=wbnamesselect[which(wbnamesselect$AppID %in% programwbselect),]
  wbnamesselectvec=wbnamesselect$AppID
  names(wbnamesselectvec)=wbnamesselect$Waterbody_Name
  return(wbnamesselectvec)
})

#Select waterbody stations
output$stationwaterbodiesselectUI = renderUI({
  pickerInput(
    inputId = "stationwaterbodiesselect",
    label = NULL,
    choices = stationwbs()
  )
})

wbstationnameselect = reactive({
  stationselect = stations()
  stationselectid = stationselect$StationID[which(stationselect$AppID == input$stationwaterbodiesselect)]
  stationselectname = stationselect$Station_Name[which(stationselect$AppID == input$stationwaterbodiesselect)]
  names(stationselectid) = stationselectname
  return(stationselectid)
})

output$aedstationsselectchoiceUI = renderUI({
  if (input$aedstations == "Add"){
    tagList(
      fluidRow(
        column(
          width = 7,
          textInput(
            inputId = "addstationname",
            label = NULL,
            placeholder = "New Station Name"
          ),
          textInput(
            inputId = "addprogramstationid",
            label = NULL,
            placeholder = "New Station ID"
          ),
          fluidRow(
            column(
              width = 6,
              textInput(
                inputId = "stationlatadd",
                label = "Latitude"
              )
            ),
            column(
              width = 6,
              textInput(
                inputId = "stationlonadd",
                label = "Longitude"
              )
            )
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "addstationbttn",
            label = "Add",
            style = "fill",
            color = "success"
          )
        )
      )
    )
  }else if (input$aedstations == "Edit"){
    tagList(
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = "editstationnamechoices",
            label = NULL,
            choices = wbstationnameselect()
          )
        )
      ),
      fluidRow(
        column(
          width = 7,
          uiOutput("editstationnameUI")
        ),
        column(
          width = 1,
          actionBttn(
            inputId = "editstationbttn",
            label = "Edit",
            style = "fill",
            color = "warning"
          )
        )
      )
    )
  }else if (input$aedstations == "Delete"){
    tagList(
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = "deletestationnamechoices",
            label = NULL,
            choices = wbstationnameselect()
          )
        ),
        column(
          width = 4,
          actionBttn(
            inputId = "deletestationbttn",
            label = "Delete",
            style = "fill",
            color = "danger"
          )
        )
      )
    )
  }
})

#Edit Waterbody Name and ID UI
output$editstationnameUI = renderUI({
  stationcoor = stations()
  
  stationcoor = stationcoor[which(stationcoor$StationID == input$editstationnamechoices),]
  tagList(
    textInput(
      inputId = "editstationname",
      label = NULL,
      value = stationcoor$Station_Name,
      placeholder = "Add Station Name"
    ),
    textInput(
      inputId = "editprogramstationid",
      label = NULL,
      value = stationcoor$ProgramStationID,
      placeholder = "Add Station ID"
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = "stationlatedit",
          label = "Latitude",
          value = stationcoor$Lat,
          placeholder = "Add Station Latitude"
        )
      ),
      column(
        width = 6,
        textInput(
          inputId = "stationlonedit",
          label = "Longitude",
          value = stationcoor$Lon,
          placeholder = "Add Station Longitude"
        )
      )
    )
  )
})

output$aedstationsdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "For the selected waterbody, add a new station, edit existing stations or delete them. A station name is required for each station. Adding
    a station id is optional. The latitude and longitude fields can be left blank.",
    HTML("</i></font>")
  )
})

#Add new station
observeEvent(
  input$addstationbttn,{
    if (nchar(input$addstationname) > 0){
      addstationname = stations()
      newstationid = random_id(1,12)
      
      stationsrow =  data.frame(
        "AppID" = input$stationwaterbodiesselect,
        "StationID" = random_id(n = 1,bytes = 12),
        "Station_Name" = input$addstationname,
        "ProgramStationID" = input$addprogramstationid,
        "Lat"=input$stationlatadd,
        "Lon"=input$stationlonadd,
        stringsAsFactors = FALSE
      )
      addstationname = rbind(addstationname,stationsrow)
      
      stations(addstationname)
      updatebaseconfig()
      
      updateTextInput(
        session = session,
        inputId = "addstationname",
        value = ""
      )
      
      updateTextInput(
        session = session,
        inputId = "addprogramstationid",
        value = ""
      )
      
      updateTextInput(
        session = session,
        inputId = "stationlatadd",
        value = ""
      )
      
      updateTextInput(
        session = session,
        inputId = "stationlonadd",
        value = ""
      )
    }else if (nchar(input$addstationname) == 0){
      sendSweetAlert(
        session = session,
        title = "Missing Station Name",
        text = "Please add a Station Name.",
        type = "danger"
      )
    }
  }
)

#Edit Station
observeEvent(
  input$editstationbttn,{
    editstations = stations()
    editstations$Station_Name[which(editstations$StationID == input$editstationnamechoices)] = input$editstationname
    editstations$ProgramStationID[which(editstations$StationID == input$editstationnamechoices)] = input$editprogramstationid
    editstations$Lat[which(editstations$StationID == input$editstationnamechoices)] = input$stationlatedit
    editstations$Lon[which(editstations$StationID == input$editstationnamechoices)] = input$stationlonedit
    
    stations(editstations)
    updatebaseconfig()
  }
)

#Delete Station
observeEvent(
  input$deletestationbttn,{
    deletestations = stations()
    deletestations = deletestations[which(deletestations$StationID != input$deletestationnamechoices),]
    stations(deletestations)
    updatebaseconfig()
  }
)