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
            isolate(
              pickerInput(
                inputId = "procprogram",
                label = "Program",
                choices = programid(),
                options = list(
                  style = "btn-success"
                )
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
  isolate(
    pickerInput(
      inputId = "procwaterbody",
      label = "Waterbody",
      choices = procwaterbody_names(),
      options = list(
        style = "btn-success"
      )
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
  
  isolate(
    pickerInput(
      inputId = "procstationname",
      choices = stationnamechoices,
      label = "Station Name",
      options = list(
        style = "btn-success"
      )
    )
  )
})

#Processing Model choices
output$procmodelUI = renderUI({
  
  #loggermodelsedit() reactive sourced from logfiledefs.R
  isolate(
    pickerInput(
      inputId = "procmodel",
      label = "Logger Model",
      choices = loggermodelsedit(),
      options = list(
        style = "btn-success"
      )
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
  
  if (nrow(deployidselect) > 0){
    if (all(is.na(deployidselect$DeployID))){
      deploycountdata = 1
    }else{
 
      deployidselect = unique(deployidselect$DeployID[which(deployidselect$StationID == input$procstationname & 
                                                          deployidselect$ModelID == input$procmodel)])
      
      if (length(deployidselect)>0){
        # print(max(deploycountdata$Deployment[which(deploycountdata$DeployID %in% deployidselect)]))
        
        deploycountdata = as.numeric(max(deploycountdata$Deployment[which(deploycountdata$DeployID %in% deployidselect)])) + 1
        
      }else{
        deploycountdata = 1
      }

    }
  }else{
    deploycountdata = 1
  }
  
  return(deploycountdata)
})

output$procmetadataUI = renderUI({
  coords = coordselect()
  deploynumber = deploycounter()
  
  tagList(
    fluidRow(
      column(
        width = 6,
        isolate(
          numericInput(
            inputId = "lat",
            label = "Latitude",
            value = coords$Lat
          )
        )
      ),
      column(
        width = 6,
        isolate(
          numericInput(
            inputId = "lon",
            label = "Longitude",
            value = coords$Lon
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        isolate(
          numericInput(
            inputId = "deploynum",
            label = "Deployment",
            value = deploynumber,
            step = 1
          )
        )
      ),
      column(
        width = 9,
        isolate(
          textInput(
            inputId = "username",
            label = "User Name"
          )
        )
      )
    ),
    actionBttn(
      inputId = "processingbttn",
      label = "Process Data",
      color = "success",
      style = "fill",
      size = "lg",
      icon = icon("desktop")
    )
  )
})

#Processing Description
output$procdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Select the relevant Program, Waterbody, Station, and Logger Model in the Setup box at the top. Each file to be processed should be named
    according to a logger unit id. This could be a Serial Number or some other unique identifier. Enter each of these identifiers into the
    Logger Units Box along with any associated Z/Depth/Altitude values (if relevant). Upload the data for processing. Ensure the Latitude,
    Longitude, Deployment, and User Name fields are correct and click the Process Data button. Wait for the progress bar to reach 100% before
    moving on to the next steps. The processed data can be viewed in the Data Preview box by clicking the + sign on the right.",
    HTML("</i></font>")
  )
})

#Data Preview Table UI
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