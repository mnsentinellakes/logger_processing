#Program Waterbody Loggers Models choices
output$pwlmUI = renderUI({
  fluidRow(
    column(
      width = 12,
      box(
        solidHeader = TRUE,
        status = "primary",
        width = NULL,
        title = "Setup",
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "procprogram",
              label = "Program",
              choices = programid(),
              options = list(
                style = "btn-success"
              )
            )
          ),
          column(
            width = 3,
            uiOutput("procwaterbodyUI")
          ),
          column(
            width = 3,
            uiOutput("procstationnameUI")
          ),
          column(
            width = 3,
            uiOutput("procmodelUI")
          )
        )
      )
    )
  )
})

#Create a vector of AppIDS that are in the selected Program
procprogramappids = reactive({
  programwbschoice=programwbs()
  programwbschoice=programwbschoice$AppID[which(programwbschoice$ProgramID == input$procprogram)]
  return(programwbschoice)
})

#Create a vector of Waterbody Names from the AppIDS
procwaterbody_names = reactive({
  wbnamesselect=wbnames()
  wbnamesselect=wbnamesselect[which(wbnamesselect$AppID %in% procprogramappids()),]
  wbnamesselectvec=wbnamesselect$AppID
  names(wbnamesselectvec)=wbnamesselect$Waterbody_Name
  
  return(wbnamesselectvec)
})

#Processing Waterbody Choices
output$procwaterbodyUI = renderUI({
  pickerInput(
    inputId = "procwaterbody",
    label = "Waterbody",
    choices = procwaterbody_names(),
    options = list(
      style = "btn-success"
    )
  )
})

#Select Station
selectstation = reactive({
  stationselect = stations()
  stationselect = stationselect[which(stationselect$AppID == input$procwaterbody),]
  return(stationselect)
})

#Processing Site Name choices
output$procstationnameUI = renderUI({
  stationnametable = selectstation()
  stationnamechoices = stationnametable$StationID
  names(stationnamechoices) = stationnametable$Station_Name
  
  pickerInput(
    inputId = "procstationname",
    choices = stationnamechoices,
    label = "Station Name",
    options = list(
      style = "btn-success"
    )
  )
})

#Processing Model choices
output$procmodelUI = renderUI({
  
  #loggermodelsedit() reactive sourced from logfiledefs.R
  pickerInput(
    inputId = "procmodel",
    label = "Logger Model",
    choices = loggermodelsedit(),
    options = list(
      style = "btn-success"
    )
  )
})

coordselect = reactive({
  stationcoord = stations()
  stationcoord = stationcoord[which(stationcoord$StationID == input$procstationname),]
  return(stationcoord)
})

#Deployment Counter Processing
deploycounter = reactive({
  deployidselect = processinglogs()
  deploycountdata = deploylogs()
  
  deployidselect = unique(deployidselect$DeployID[which(deployidselect$AppID == input$procwaterbody & deployidselect$Logger_Type == input$proclogger)])
  

  if (length(deployidselect)>0){
  
  deploycountdata = max(deploycountdata$Deployment[which(deploycountdata$DeployID %in% deployidselect)])
  }else{
    deploycountdata = 1
  }
  return(deploycountdata)
})


output$procmetadata=renderUI({
  coords = coordselect()
  
  tagList(
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = "lat",
          label = "Latitude",
          value = coords$Lat
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = "lon",
          label = "Longitude",
          value = coords$Lon
        )
      )
    ),
    textInput(
      inputId = "deploynum",
      label = "Deployment",
      value = deploycounter()
    )
  )
})

output$dataprevUI = renderUI({
  tagList(
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "prevloggerchoices",
          label = "Data Type",
          choices = qcloggertypes(),
        )
      )
    ),
    DTOutput("dataoutput")
  )
})