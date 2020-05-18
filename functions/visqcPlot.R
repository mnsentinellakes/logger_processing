#Plot Data for Visual QC---------------
output$VisQCplot=renderPlotly({
  QCdata=VisQCdata()
  QCdata=QCdata[QCdata$Serial_Number == input$siteidchoice,]
  
  flagvcolors=c("P"="navyblue",
                "F"="red",
                "S"="orange")
  if (input$Type=="Temperature"){
    plot_ly(
      source = "VQCselect",
      x=QCdata$Date_Time,
      y=QCdata$Water_Temp_C,
      type = "scattergl",
      key = QCdata$Date_Time,
      symbols = 16,
      color = QCdata$FlagV,
      colors = flagvcolors,
      mode = "markers"
    ) %>%
      layout(
        xaxis = list(
          title = "Date",
          showticklabels=FALSE), 
        yaxis = list(
          title = "Temperature"
        )
      )
  }else if (input$Type=="Dissolved Oxygen"){
    validate(
      need(input$DOmeasure,"Loading...")
    )
    if (input$DOmeasure=="DO"){
      plot_ly(
        source = "VQCselect",
        x=QCdata$Date_Time,
        y=QCdata$DO,
        type = "scattergl",
        key = QCdata$Date_Time,
        symbols = 16,
        color = QCdata$DO_FlagV,
        colors = flagvcolors,
        mode = "markers"
      )%>%
        layout(
          xaxis = list(
            title = "Date",
            showticklabels=FALSE), 
          yaxis = list(
            title = "DO"
          )
        )
    }else if(input$DOmeasure=="Temperature"){
      plot_ly(
        source = "VQCselect",
        x=QCdata$Date_Time,
        y=QCdata$Water_Temp_C,
        type = "scattergl",
        key = QCdata$Date_Time,
        symbols = 16,
        color = QCdata$Temp_FlagV,
        colors = flagvcolors,
        mode = "markers"
      )%>%
        layout(
          xaxis = list(
            title = "Date",
            showticklabels=FALSE
          ), 
          yaxis = list(
            title = "Temperature"
          )
        )
    }
  }
})

#FlagV=Fail for selected data-----
observeEvent(
  input$visfail,{
    VQCfaildataF=event_data(
      event = "plotly_selected",
      source = "VQCselect"
    )
    VQCFail=VisQCdata()
    if (input$Type=="Temperature"){
      VQCFail$FlagV[which(VQCFail$Serial_Number==input$siteidchoice & VQCFail$Date_Time %in% VQCfaildataF$key)]="F"
    }else if (input$Type=="Dissolved Oxygen"){
      if (input$DOmeasure=="DO"){
        VQCFail$DO_FlagV[which(VQCFail$Serial_Number==input$siteidchoice & VQCFail$Date_Time %in% VQCfaildataF$key)]="F"
      }else if (input$DOmeasure=="Temperature"){
        VQCFail$Temp_FlagV[which(VQCFail$Serial_Number==input$siteidchoice & VQCFail$Date_Time %in% VQCfaildataF$key)]="F"
      }
    }
    VisQCdata(VQCFail)
  })

#FlagV=Suspect for selected data------
observeEvent(
  input$vissusp,{
    VQCfaildataS=event_data(
      event = "plotly_selected",
      source = "VQCselect"
    )
    VQCSusp=VisQCdata()
    if (input$Type=="Temperature"){
      VQCSusp$FlagV[which(VQCSusp$Serial_Number==input$siteidchoice & VQCSusp$Date_Time %in% VQCfaildataS$key)]="S"
    }else if (input$Type=="Dissolved Oxygen"){
      if (input$DOmeasure=="DO"){
        VQCSusp$DO_FlagV[which(VQCSusp$Serial_Number==input$siteidchoice & VQCSusp$Date_Time %in% VQCfaildataS$key)]="S"
      }else if (input$DOmeasure=="Temperature"){
        VQCSusp$Temp_FlagV[which(VQCSusp$Serial_Number==input$siteidchoice & VQCSusp$Date_Time %in% VQCfaildataS$key)]="S"
      }
    }
    VisQCdata(VQCSusp)
  })

#FlagV=Pass for selected data-----
observeEvent(
  input$vispass,{
    VQCfaildataP=event_data(
      event = "plotly_selected",
      source = "VQCselect")
    VQCPass=VisQCdata()
    if (input$Type=="Temperature"){
      VQCPass$FlagV[which(VQCPass$Serial_Number==input$siteidchoice & VQCPass$Date_Time %in% VQCfaildataP$key)]="P"
    }else if (input$Type=="Dissolved Oxygen"){
      if (input$DOmeasure=="DO"){
        VQCPass$DO_FlagV[which(VQCPass$Serial_Number==input$siteidchoice & VQCPass$Date_Time %in% VQCfaildataP$key)]="P"
      }else if (input$DOmeasure=="Temperature"){
        VQCPass$Temp_FlagV[which(VQCPass$Serial_Number==input$siteidchoice & VQCPass$Date_Time %in% VQCfaildataP$key)]="P"
      }
    }
    VisQCdata(VQCPass)
  })