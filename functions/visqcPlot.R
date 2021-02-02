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
    need(!is.null(VisQCdata()),"Loading...")
  )
  #datatypedf() from visqcUI code file
  QCdata = datatypedf()
  QCdata = QCdata[which(QCdata$UnitID == input$unitidchoice),]
  
  flagfield = sym(flagtype())
  flagcolors=c("P"="royalblue1",
               "F"="red",
               "S"="orange",
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
    toWebGL(
      ggplotly(
        dynamicTicks = TRUE,
        source = "V",
        ggplot(
          data = QCdata
        ) +
          geom_point(
            aes(
              x = DateTime,
              y = Data,
              color = !!flagfield,
              key = DateTime
            ),
            size = 2.5
          ) +
          xlab("Date and Time") +
          ylab(input$visqcloggerchoices) +
          scale_color_manual(values = flagcolors) +
          Theme
      )
    )
  )
})

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
    #datatypedf() from visqcUI code file
    VQCFail = datatypedf()
    VQCFail[which(VQCFail$UnitID == input$unitidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),flagtype()] = "F"
    VQCFail[which(VQCFail$UnitID == input$unitidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),paste0(flagtype(),"chng")] = "F"
    VisQCdataupdate(VQCFail)
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
    VisQCdataupdate(VQCSusp)
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
    VisQCdataupdate(VQCPass)
  }
)