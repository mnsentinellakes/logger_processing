#Export Data----

#Updated Valuebox from: https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
# valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
#                           icon = NULL, color = "aqua", width = 4, href = NULL){
#   shinydashboard:::validateColor(color)
#   
#   if (!is.null(icon))
#     shinydashboard:::tagAssert(icon, type = "i")
#   
#   info_icon <- tags$small(
#     tags$i(
#       class = "fa fa-info-circle fa-lg",
#       title = info,
#       `data-toggle` = "tooltip",
#       style = "color: rgba(255, 255, 255, 0.75);"
#     ),
#     # bs3 pull-right 
#     # bs4 float-right
#     class = "pull-right float-right"
#   )
#   
#   boxContent <- div(
#     class = paste0("small-box bg-", color),
#     div(
#       class = "inner",
#       tags$small(title),
#       if (!is.null(sparkobj))
#       h3(value),
#       if (!is.null(sparkobj)) sparkobj,
#       p(subtitle)
#     ),
#     # bs3 icon-large
#     # bs4 icon
#     if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
#   )
#   
#   if (!is.null(href)) 
#     boxContent <- a(href = href, boxContent)
#   
#   div(
#     class = if (!is.null(width)) paste0("col-sm-", width), 
#     boxContent
#   )
# }
# 
summaryloggertypes = reactive({
  summaryloggernames = VisQCdata()
  summaryloggernames = names(summaryloggernames)
  return(summaryloggernames)
})

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

output$exportUI = renderUI({
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
  
  tablestyletop = "padding-right: 20px; padding-left: 10px; padding-bottom: 10px; border-right: 1px solid #3c8dbc"
  tablestyleendtop = "padding-left: 10px; padding-bottom: 10px;"
  tablestylebottom = "padding-right: 20px; padding-left:10px; border-right: 1px solid #3c8dbc"
  tablestyleendbottom = "padding-left: 10px"
  
  deploydata = deploylogs()
  deploydata = deploydata[which(deploydata$DeployID == deployid()),]
  filestartdate = deploydata$StartDateTimeRecord
  fileenddate = deploydata$EndDateTimeRecord
  datastartdate = deploydata$StartDateTimeValid
  dataenddate = deploydata$EndDateTimeValid
  
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
            style = tablestyleendtop,
            HTML("<font size = 3>Logger Model</font>")
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
            style = tablestyleendbottom,
            HTML(paste("<font size = 4><b>",loggermodeltype,"</b></font>"))
          )
        )
      )
    ),
    box(
      title = NULL,
      status = "success",
      width = 8,
      fluidRow(
        column(
          width = 6,
          tags$table(
            tags$tr(
              tags$td(
                style = tablestyletop,
                HTML("<font size = 3>Data Start</font>")
              ),
              tags$td(
                style = tablestyleendtop,
                HTML("<font size = 3>Valid Data Start</font>")
              )
            ),
            tags$tr(
              tags$td(
                style = tablestylebottom,
                HTML(paste("<font size = 4>",filestartdate,"</font>"))
              ),
              tags$td(
                style = tablestyleendbottom,
                HTML(paste("<font size = 4>",datastartdate,"</font>"))
              )
            )
          )
        ),
        column(
          width = 6,
          tags$table(
            tags$tr(
              tags$td(
                style = tablestyletop,
                HTML("<font size = 3>Valid Data End</font>")
              ),
              tags$td(
                style = tablestyleendtop,
                HTML("<font size = 3>Data End</font>")
              )
            ),
            tags$tr(
              tags$td(
                style = tablestylebottom,
                HTML(paste("<font size = 4>",dataenddate,"</font>"))
              ),
              tags$td(
                style = tablestyleendbottom,
                HTML(paste("<font size = 4>",fileenddate,"</font>"))
              )
            )
          )
        )
      )
    ),
    box(
      title = NULL,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          width = 2,
          pickerInput(
            inputId = "summaryloggerchoices",
            choices = summaryloggertypes(),
            label = "Data Type"
          ),
          uiOutput("summarysnchoicesUI")
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


summarydatatypes = reactive({
  # validate(
  #   need(input$summaryloggerchoices,"Loading...")
  # )
  
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
    label = "Serial Numbers"
  )
})

summaryselectdata = reactive({
  summaryselect = summarydatatypes()
  summaryselect = summaryselect[which(summaryselect$SiteId == input$summarysnchoices),]
  
  return(summaryselect)
})


valueBoxFlags = function(inputdata,flagfield,status){
  
  print(paste(flagfield,status,"1"))
  if (status == 'F'){
    statusupdate = c('F','X')
  }else{
    statusupdate = status
  }
  
  print(paste(flagfield,status,"2"))
  
  if (flagfield == "Gross"){
    inputdataselect = inputdata[which(inputdata$FlagGross %in% statusupdate),]
  }else if (flagfield == "Spike"){
    inputdataselect = inputdata[which(inputdata$FlagSpike %in% statusupdate),]
  }else if (flagfield == "RoC"){
    inputdataselect = inputdata[which(inputdata$RoC %in% statusupdate),]
  }else if (flagfield == "Flat"){
    inputdataselect = inputdata[which(inputdata$FlagFlat %in% statusupdate),]
  }else if (flagfield == "Vis"){
    inputdataselect = inputdata[which(inputdata$FlagVis %in% statusupdate),]
  }
  
  print(paste(flagfield,status,"3"))
  
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
  
  print(paste(flagfield,status,"4"))
  
  if (nrow(inputdataselect) > 0){
    flagcount = nrow(inputdataselect)
    
    percflag = round((flagcount / nrow(inputdataselect)) * 100,digits = 2)
    
    print(paste(flagfield,status,"5.1"))
  }else{
    flagcount = 0
    percflag = 0
    
    print(paste(flagfield,status,"5.2"))
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
  print(paste(flagfield,status,"6"))
    return(outputvb)
}

output$vbfail = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Gross",
      status = "F"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Spike",
      status = "F"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "RoC",
      status = "F"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Flat",
      status = "F"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Vis",
      status = "F"
    )
  )
})

output$vbsuspect = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Gross",
      status = "S"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Spike",
      status = "S"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "RoC",
      status = "S"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Flat",
      status = "S"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Vis",
      status = "S"
    )
  )
})

output$vbpass = renderUI({
  tagList(
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Gross",
      status = "P"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Spike",
      status = "P"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "RoC",
      status = "P"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Flat",
      status = "P"
    ),
    valueBoxFlags(
      inputdata = summaryselectdata(),
      flagfield = "Vis",
      status = "P"
    )
  )
})
