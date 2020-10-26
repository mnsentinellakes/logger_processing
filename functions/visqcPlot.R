#Plot Data for Visual QC---------------
# VQCselect = reactiveVal()

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
  QCdata = datatypedf()
  
  QCdata = QCdata[which(QCdata$SiteId == input$siteidchoice),]

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
    # panel.ontop = T,
    legend.key = element_blank(),
    legend.key.height = unit(20,"mm"),
    legend.key.width = unit(12,"mm")
  )
  
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
})

VisQCdataupdate = function(updatedata){
  VisQCdataselect = VisQCdata()
  VisQCdataselect[[input$visqcloggerchoices]] = updatedata
  VisQCdata(VisQCdataselect)
}

#Update the deployment table dates
deploydatesupdate = function(){
  
  deployqcdata = datatypedf()
  deployupdate = deploylogs()
  
  minrecorddt = min(deployqcdata$DateTime)
  maxrecorddt = max(deployqcdata$DateTime)
  minvaliddt = min(deployqcdata$DateTime[which(deployqcdata$FlagVis == 'P')])
  maxvaliddt = max(deployqcdata$DateTime[which(deployqcdata$FlagVis == 'P')])
  
  message(paste("StartDateTimeRecord = ",minrecorddt))
  message(paste("EndDateTimeRecord = ",maxrecorddt))
  message(paste("StartDateTimeValid = ",minvaliddt))
  message(paste("EndDateTimeValid = ",maxvaliddt))
  
  deployupdate$StartDateTimeRecord[which(deployupdate$DeployID == deployid())] = minrecorddt
  deployupdate$EndDateTimeRecord[which(deployupdate$DeployID == deployid())] = maxrecorddt
  deployupdate$StartDateTimeValid[which(deployupdate$DeployID == deployid())] = minvaliddt
  deployupdate$EndDateTimeValid[which(deployupdate$DeployID == deployid())] = maxvaliddt
  
  deploylogs(deployupdate)
  
  updatebaseconfig()
}

#FlagV=Fail for selected data-----
observeEvent(
  input$fail,{
    VQCfaildataF = event_data(
      event = "plotly_selected",
      source = "V"
    )
    VQCFail = datatypedf()
    VQCFail[which(VQCFail$SiteId == input$siteidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),flagtype()] = "F"
    VQCFail[which(VQCFail$SiteId == input$siteidchoice & as.character(VQCFail$DateTime) %in% VQCfaildataF$key),paste0(flagtype(),"chng")] = "F"
    VisQCdataupdate(VQCFail)
    deploydatesupdate()
  })

#FlagV=Suspect for selected data------
observeEvent(
  input$susp,{
    VQCfaildataS = event_data(
      event = "plotly_selected",
      source = "V"
    )
    VQCSusp = datatypedf()
    VQCSusp[which(VQCSusp$SiteId == input$siteidchoice & as.character(VQCSusp$DateTime) %in% VQCfaildataS$key),flagtype()] = "S"
    VQCSusp[which(VQCSusp$SiteId == input$siteidchoice & as.character(VQCSusp$DateTime) %in% VQCfaildataS$key),paste0(flagtype(),"chng")] = "S"
    VisQCdataupdate(VQCSusp)
    deploydatesupdate()
  })

#FlagV=Pass for selected data-----
observeEvent(
  input$pass,{
    VQCfaildataP = event_data(
      event = "plotly_selected",
      source = "V")
    VQCPass = datatypedf()
    VQCPass[which(VQCPass$SiteId == input$siteidchoice & as.character(VQCPass$DateTime) %in% VQCfaildataP$key),flagtype()] = "P"
    VQCPass[which(VQCPass$SiteId == input$siteidchoice & as.character(VQCPass$DateTime) %in% VQCfaildataP$key),paste0(flagtype(),"chng")] = "P"
    VisQCdataupdate(VQCPass)
    deploydatesupdate()
  })