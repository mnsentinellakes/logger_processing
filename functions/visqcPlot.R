#Plot Data for Visual QC---------------

flagtype = reactive({
  if(input$flagselect == "Visual"){
    flagfield = "FlagVis"
  }else if (input$flagselect == "Gross"){
    flagfield = "FlagGross"
  }else if (input$flagselect == "Spike"){
    flagfield = "FlagSpike"
  }else if (input$flagselect == "Rate of Change"){
    flagfield = "FlagRoC"
  }else if (input$flagselect == "Flat"){
    flagfield = "FlagFlat"
  }
  return(flagfield)
})

output$VisQCplot = renderPlotly({
  validate(
    need(!is.null(VisQCdata()),"Loading..."),
    need(!is.null(export()),"Loading..."),
    need(input$unitidchoice,"Loading..."),
    need(input$procprogram,"Loading..."),
    need(input$procmodel,"Loading..."),
    need(input$flagselect,"Loading...")
  )
  #datatypedf() from visqcUI code file
  QCdata = datatypedf()
  QCdata = QCdata[which(QCdata$UnitID == input$unitidchoice),]

  incexportnotes = export()
  incexportnotes = incexportnotes$IncNotes[which(incexportnotes$ProgramID == input$procprogram & incexportnotes$ModelID == input$procmodel)]
  
  flagfield = sym(flagtype())
  flagcolors = c("P" = "royalblue1",
                 "F" = "red",
                 "S" = "orange",
                 "X" = "slategray3")
  
  Theme = theme(
    panel.border = element_rect(color = "black",fill = NA,size = 1),
    panel.grid.major.x = element_line(color = "lightgray",size = 0.5,linetype = 3),
    panel.grid.major.y = element_line(color = "lightgray",size = 0.5,linetype = 3),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.key = element_blank(),
    legend.key.height = unit(20,"mm"),
    legend.key.width = unit(12,"mm")
  )

  suppressWarnings(
    if (incexportnotes == TRUE){
      p = geom_point(
        aes(
          x = DateTime,
          y = Data,
          color = !!flagfield,
          key = DateTime,
          text = paste(
            "DateTime:",DateTime,
            "\nData:",Data,
            "\nFlag:",input$flagselect,!!flagfield,
            "\nNotes:",Notes
          )
        ),
        size = 2.5
      )
    }else{
      p = geom_point(
        aes(
          x = DateTime,
          y = Data,
          color = !!flagfield,
          key = DateTime,
          text = paste0(
            "DateTime: ",DateTime,
            "\nData: ",Data,
            "\nFlag ",input$flagselect,": ",!!flagfield

          )
        ),
        size = 2.5
      )
    }
  )
  suppressWarnings(
    toWebGL(
      ggplotly(
        dynamicTicks = TRUE,
        tooltip = "text",
        source = "V",
        ggplot(
          data = QCdata
        ) +
          p +
          xlab("Date and Time") +
          ylab(input$visqcloggerchoices) +
          scale_color_manual(values = flagcolors) +
          Theme
      ) %>%
        config(displayModeBar = TRUE)
    )
  )
})

# #Reactively update browser dimensions
browserdim = reactive({
  dimensiondata=c(input$dimension[1],input$dimension[2])
  dimensiondata
})

#Toggle whether the Notes box should be displayed
notestoggle = reactive({
  exportrow = export()
  notes = exportrow$IncNotes[which(exportrow$ProgramID == input$procprogram & exportrow$ModelID == input$procmodel)]
  multiloggers = exportrow$FileSep[which(exportrow$ProgramID == input$procprogram & exportrow$ModelID == input$procmodel)]
  outputtoggle = c(notes,multiloggers)
  return(outputtoggle)
})

output$VisQCplotUI = renderUI({
  validate(
    need(input$procprogram,"Loading..."),
    need(input$procmodel,"Loading...")
  )

  #Import export table for dimensions
  if(notestoggle()[1] == FALSE ){
    dimdiv = 1.25
  }else{
    dimdiv = 1.75
  }
  tagList(
    plotlyOutput("VisQCplot",height = browserdim()[2]/dimdiv)
  )
})

#Data notes entry values
notesentry = reactive({
  
  qcnotesdata = datatypedf()
  if ("Notes" %in% colnames(qcnotesdata)){
    notesselectdata = event_data(
      event = "plotly_selected",
      source = "V"
    )

    selectednotes = unique(qcnotesdata$Notes[which(qcnotesdata$UnitID == input$unitidchoice & as.character(qcnotesdata$DateTime) %in% notesselectdata$key)])
    
    if (length(selectednotes) > 1){
      selectednotes = "WARNING: multiple data points with different notes selected"
    }
    return(selectednotes)
  }
})


#Box for notes if needed
output$notesentryUI = renderUI({
  validate(
    need(input$procprogram,"Loading..."),
    need(input$procmodel,"Loading...")
  )
  
  if (notestoggle()[1] == TRUE){
    box(
      title = "Notes",
      solidHeader = TRUE,
      status = "primary",
      width = NULL,
      textAreaInput(
        inputId = "snnotes",
        value = notesentry(),
        label = NULL,
        placeholder = "Enter Notes"
      )
    )
  }
})

#Update data
VisQCdataupdate = function(updatedata){
  VisQCdataselect = VisQCdata()
  VisQCdataselect[[input$visqcloggerchoices]] = updatedata
  VisQCdata(VisQCdataselect)
}

#FlagV=Fail for selected data-----
observeEvent(
  input$failbttn,{
    VQCfaildataF = event_data(
      event = "plotly_selected",
      source = "V"
    )
    
    VQCFail = datatypedf()
    VQCFail[which(VQCFail$UnitID == input$unitidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),flagtype()] = "F"
    VQCFail[which(VQCFail$UnitID == input$unitidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),paste0(flagtype(),"chng")] = "F"
    
    #Update Notes
    VQCFail$Notes[which(VQCFail$UnitID == input$unitidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key)] = input$snnotes
    VisQCdataupdate(VQCFail)
    
    #Clear Notes text box
    updateTextAreaInput(
      session,
      inputId = "snnotes",
      label = NULL,
      value = ""
    )
  }
)

#FlagV=Suspect for selected data------
observeEvent(
  input$suspbttn,{
    VQCfaildataS = event_data(
      event = "plotly_selected",
      source = "V"
    )
    #datatypedf() from visqcUI code file
    VQCSusp = datatypedf()
    VQCSusp[which(VQCSusp$UnitID == input$unitidchoice & as.character(VQCSusp$DateTime) %in% VQCfaildataS$key),flagtype()] = "S"
    VQCSusp[which(VQCSusp$UnitID == input$unitidchoice & as.character(VQCSusp$DateTime) %in% VQCfaildataS$key),paste0(flagtype(),"chng")] = "S"
    
    #Update Notes
    VQCSusp$Notes[which(VQCSusp$UnitID == input$unitidchoice & as.character(VQCSusp$DateTime) %in% VQCfaildataS$key)] = input$snnotes
    VisQCdataupdate(VQCSusp)
    
    #Clear Notes text box
    updateTextAreaInput(
      session,
      inputId = "snnotes",
      label = NULL,
      value = ""
    )
  }
)

#FlagV=Pass for selected data-----
observeEvent(
  input$passbttn,{
    VQCfaildataP = event_data(
      event = "plotly_selected",
      source = "V"
    )
    #datatypedf() from visqcUI code file
    VQCPass = datatypedf()
    VQCPass[which(VQCPass$UnitID == input$unitidchoice & as.character(VQCPass$DateTime) %in% VQCfaildataP$key),flagtype()] = "P"
    VQCPass[which(VQCPass$UnitID == input$unitidchoice & as.character(VQCPass$DateTime) %in% VQCfaildataP$key),paste0(flagtype(),"chng")] = "P"
    
    #update Notes
    VQCPass$Notes[which(VQCPass$UnitID == input$unitidchoice & as.character(VQCPass$DateTime) %in% VQCfaildataP$key)] = input$snnotes
    VisQCdataupdate(VQCPass)
    
    #Clear Notes text box
    updateTextAreaInput(
      session,
      inputId = "snnotes",
      label = NULL,
      value = ""
    )
  }
)