#Export Data----

#Updated Valuebox from: https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/



valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

valueBoxFlags = function(inputdata,flagfield,status){
  
  if (flagfield == "Gross"){
    if (status == 'F'){
      inputdataselect = inputdata[which(inputdata$FlagGross == 'F' | inputdata$FlagGross == 'X'),]
    }else{
      inputdataselect = inputdata[which(inputdata$FlagGross == status),]
    }
  }else if (flagfield == "Spike"){
    if (status == 'F'){
      inputdataselect = inputdata[which(inputdata$FlagSpike == 'F' | inputdata$FlagSpike == 'X'),]
    }else{
      inputdataselect = inputdata[which(inputdata$FlagSpike == status),]
    }
  }else if (flagfield == "RoC"){
    if (status == 'F'){
      inputdataselect = inputdata[which(inputdata$FlagRoC == 'F' | inputdata$FlagRoC == 'X'),]
    }else{
      inputdataselect = inputdata[which(inputdata$FlagRoC == status),]
    }
  }else if (flagfield == "Flat"){
    if (status == 'F'){
      inputdataselect = inputdata[which(inputdata$FlagFlat == 'F' | inputdata$FlagFlat == 'X'),]
    }else{
      inputdataselect = inputdata[which(inputdata$FlagFlat == status),]
    }
  }else if (flagfield == "Vis"){
    if (status == 'F'){
    inputdataselect = inputdata[which(inputdata$FlagVis == 'F' | inputdata$FlagVis == 'X'),]
    }else{
      inputdataselect = inputdata[which(inputdata$FlagVis == status),]
    }
  }
  
  if (status == 'F'){
    statusname = "Fail"
    statuscolor = "red"
    statusicon = icon("times-circle")
  }else if (status == 'S'){
    statusname = "Suspect"
    statuscolor = "orange"
    statusicon = icon("question-circle")
  }else if (status == 'P'){
    statusname = "Pass"
    statuscolor = "blue"
    statusicon = icon("thumbs-up")
  }
  
  if (nrow(inputdataselect) > 0){
    flagcount = nrow(inputdataselect)
    percflag = round(((flagcount / nrow(inputdata)) * 100),digits = 4)
  }else{
    flagcount = 0
    percflag = 0
  }
  
  outputvb = valueBox2(
    value = flagcount,
    title = paste("Flag",flagfield,statusname),
    subtitle = paste0(percflag,"% of the data"),
    icon = statusicon,
    color = statuscolor,
    width = NULL,
    href = NULL
  )
  
  return(outputvb)
}

summaryloggertypes = reactive({
  summaryloggernames = VisQCdata()
  summaryloggernames = names(summaryloggernames)
  return(summaryloggernames)
})

#Table CSS
tablestyletop = "padding-right: 20px; padding-left: 10px; padding-bottom: 10px; border-right: 1px solid #3c8dbc"
tablestyleendtop = "padding-left: 10px; padding-bottom: 10px;"
tablestylebottom = "padding-right: 20px; padding-left:10px; border-right: 1px solid #3c8dbc"
tablestyleendbottom = "padding-left: 10px"

#Main UI
output$summaryUI = renderUI({
  validate(
    need(summaryloggertypes(),"Loading...")
  )
  
  #Get Program Name
  programname = programs()
  programname = programname$Program_Name[which(programname$ProgramID == input$procprogram)]
  
  #Get Waterbody Name
  waterbodyname = wbnames()
  waterbodyname = waterbodyname$Waterbody_Name[which(waterbodyname$AppID == input$procwaterbody)]
  
  #Get Waterbody ID
  waterbody = programwbs()
  waterbodyid = waterbody$ProgramWaterbodyID[which(waterbody$AppID == input$procwaterbody)]
  
  #Get Waterbody Type
  waterbodytype = waterbody$WB_Type[which(waterbody$AppID == input$procwaterbody)]
  
  #Get Station Info
  station = stations()
  stationname = station$Station_Name[which(station$StationID == input$procstationname)]
  stationlat = as.character(station$Lat[which(station$StationID == input$procstationname)])
  stationlon = as.character(station$Lon[which(station$StationID == input$procstationname)])
  
  if ((length(stationlat) > 0 & length(stationlon) > 0) | !is.na(stationlat) & !is.na(stationlon)){
    loc = paste("<font size = 4><b>",stationlat,",",stationlon,"</b></font>")
  }else{
    loc = ""
  }
  
  #Logger Model Type
  loggermodeltype = loggerfiledefs()
  loggermodeltype = loggermodeltype$Logger_Model[which(loggermodeltype$ModelID == input$procmodel)]
  
  deploydata = deploylogs()
  deploydata = deploydata[which(deploydata$DeployID == deployid()),]
  deploycount = deploydata$Deployment_Count
  loggercount = deploydata$Logger_Count

  
  tagList(
    box(
      title = NULL,
      status = "primary",
      width = 12,
      tags$table(
        tags$tr(
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Program</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Waterbody</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>ID</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Type</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Station</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Location</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Logger Model</font>")
          ),
          tags$td(
            style = tablestyletop,
            HTML("<font size = 3>Deployment Number</font>")
          ),
          tags$td(
            style = tablestyleendtop,
            HTML("<font size = 3>Logger Count</font>")
          )
        ),
        tags$tr(
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",programname,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",waterbodyname,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",waterbodyid,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",waterbodytype,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",stationname,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(loc)
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",loggermodeltype,"</b></font>"))
          ),
          tags$td(
            style = tablestylebottom,
            HTML(paste("<font size = 4><b>",deploycount,"</b></font>"))
          ),
          tags$td(
            style = tablestyleendbottom,
            HTML(paste("<font size = 4><b>",loggercount,"</b></font>"))
          )
        )
      )
    ),
    box(
      title = NULL,
      status = "success",
      width = 12,
      fluidRow(
        column(
          width = 2,
          pickerInput(
            inputId = "summaryloggerchoices",
            choices = summaryloggertypes(),
            label = "Data Type",
            options = list(
              style = "btn-primary")
          )
          ),
        column(
          width = 2,
          uiOutput("summarysnchoicesUI")
        ),
        column(
          width = 7,
          uiOutput("summaryloggerinfoUI")

        ),
      )
    ),
    box(
      title = NULL,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          width = 1,
          radioGroupButtons(
            inputId = "flagtype",
            label = NULL,
            choiceNames = c("Gross","Spike","Rate of Change","Flat","Visual"),
            choiceValues = c("Gross","Spike","RoC","Flat","Vis"),
            status = "primary",
            checkIcon = list(
              yes = icon("check-square"),
              no = icon("square")
            ),
            direction = "vertical"
          )
        ),
        column(
          width = 1
        ),
        column(
          width = 3,
          uiOutput("vbfail")
        ),
        column(
          width = 3,
          uiOutput("vbsuspect")
        ),
        column(
          width = 3,
          uiOutput("vbpass")
        )
      )
    )
  )
})

output$summaryloggerinfoUI = renderUI({
  
  validate(
    need(summaryselectdata(),"Loading...")
  )
  
  summaryinfo = summaryselectdata()
  
  summaryfilestartdate = unique(min(summaryinfo$DateTime))
  summaryfileenddate = unique(max(summaryinfo$DateTime))
  summarydatastartdate = unique(min(summaryinfo$DateTime[which(summaryinfo$FlagVis == 'P')]))
  summarydataenddate = unique(max(summaryinfo$DateTime[which(summaryinfo$FlagVis == 'P')]))
  summarydepth = unique(summaryinfo$Depth)
  
  tags$table(
    tags$tr(
      tags$td(
        style = tablestyletop,
        HTML("<font size = 3>Depth</font>")
      ),
      tags$td(
        style = tablestyletop,
        HTML("<font size = 3 color = red>Data Start</font>")
      ),
      tags$td(
        style = tablestyletop,
        HTML("<font size = 3 color = blue>Valid Data Start</font>")
      ),
      tags$td(
        style = tablestyletop,
        HTML("<font size = 3 color = blue>Valid Data End</font>")
      ),
      tags$td(
        style = tablestyleendtop,
        HTML("<font size = 3 color = red>Data End</font>")
      )
    ),
    tags$tr(
      tags$td(
        style = tablestylebottom,
        HTML(paste("<font size = 4><CENTER>",summarydepth,"</CENTER></font>"))
      ),
      tags$td(
        style = tablestylebottom,
        HTML(paste("<font size = 4 color = red>",summaryfilestartdate,"</font>"))
      ),
      tags$td(
        style = tablestylebottom,
        HTML(paste("<font size = 4 color = blue>",summarydatastartdate,"</font>"))
      ),
      tags$td(
        style = tablestylebottom,
        HTML(paste("<font size = 4 color = blue>",summarydataenddate,"</font>"))
      ),
      tags$td(
        style = tablestyleendbottom,
        HTML(paste("<font size = 4 color = red>",summaryfileenddate,"</font>"))
      )
    )
  )
})
  
  
summarydatatypes = reactive({

  summarydataselect = VisQCdata()
  print(paste("VisQCdata names:",names(summarydataselect)))
  print(paste("input$summaryloggerchoices:",input$summaryloggerchoices))
  summarydata = summarydataselect[[input$summaryloggerchoices]]
  
  return(summarydata)
})

output$summarysnchoicesUI = renderUI({

  summarysn = summarydatatypes()
  summarysn = unique(summarysn$SiteId)
  
  pickerInput(
    inputId = "summarysnchoices",
    choices = summarysn,
    label = "Serial Number",
    options = list(
      style = "btn-primary")
  )
})

summaryselectdata = reactive({
  summaryselect = summarydatatypes()
  summaryselect = summaryselect[which(summaryselect$SiteId == input$summarysnchoices),]
  
  return(summaryselect)
})

output$vbfail = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = input$flagtype,
      status = "F"
    )
  )
})

output$vbsuspect = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = input$flagtype,
      status = "S"
    )
  )
})

output$vbpass = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = input$flagtype,
      status = "P"
    )
  )
})
