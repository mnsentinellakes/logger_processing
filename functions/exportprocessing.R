output$exportUI = renderUI({
  
  box(
    title = "Export Data",
    solidHeader = TRUE,
    width = 12,
    actionBttn(
      inputId = "exportprocessbttn",
      label = "Process Data for Export",
      style = "fill",
      color = "success",
      size = "lg"
    )
  )
  
})

# observeEvent(
#   input$exportprocessbttn,
#   {
#     x = VisQCdata()
#     save(x,file = "Test/visqcdata.R")
#   }
# )

#Select the relevant row from the export table
selectexportdata = eventReactive(
  input$exportprocessbttn,
  {
    exportdata = export()
    #input$procprogram and input$procmodel are sourced from the processingUI.R file
    exportdata = exportdata[which(exportdata$ProgramID == input$procprogram & exportdata$ModelID == input$procmodel),]
    return(exportdata)
  })

# load("C:/Projects/Shiny_App_Development/Logger_Processing/Test/visqcdata.R")



#Process data for export
processexportdata = eventReactive(
  input$exportprocessbttn,
  {
    validate(
      need(selectexportdata(),"Loading...")
    )
    #Export Data Settings
    exportsettings = selectexportdata()
    # exportsettings = export
    #Export Data
    processdata = VisQCdata()
    # processdata = x
    
    ###Setup definition rows first
    firstfields = paradata[[1]]
    firstfields = paradata[,c(1:2)]
    
    #Rename UnitID Field
    names(firstfields)[1] = exportsettings$UnitID
    
    #Format Date Fields
    firstfields[2] = with_tz(
      time = firstfields[2],
      tzone = exportsettings$TZ
    )
    
    if (exportsettings$DateTimeSep == "Combined"){
      names(firstfields)[2] = exportsettings$Date_Time
    }else if (exportsettings$DateTimeSep == "Separate"){
      firstfields$date = as.Date(firstfields[,2])
      names(firstfields)[3] = exportsettings$Date
      firstfields$time = format(firstfields[,2], format = "%H:%M:%S")
      names(firstfields)[4] = exportsettings$Time
      firstfields[,2] = NULL
    }

    
    
    for (i in qcloggertypes()){
      i = "WaterTemp"
      #Select data by logger type
      paradata = processdata[[i]]
      #Remove extra QC Flag Fields
      paradata = paradata[,-c(5:14)]
      
      #Change the name of the UnitID field (SiteID in pre-named data) or delete the field
      if (exportsettings$IncUnitID == TRUE){
        colnames(paradata)[1] = exportsettings$UnitID
      }else{
        paradata[,1] = NULL
      }
      
      #Format Date Time
      if (exportsettings$DateTimeSep == "Combined"){
        colnames(paradata)[2] = exportsettings$Date_Time
        paradata[,2] = with_tz(paradata[,2],tz = exportsettings$TZ)
        
        
      }else{
        
        
        
      }
      
    }
      
      
})