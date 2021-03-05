#Update object names to reflect qcconfig usage
savestatus = reactiveVal("")

# #Create a named vector of ProgramIDs and assign appropriate names to render selectprogramsUI
# programid = reactive({
#   programselect = programs()
#   programselectvec = programselect$ProgramID
#   names(programselectvec) = programselect$Program_Name
#   return(programselectvec)
# })

#Render the UI for selecting the program
output$selectprogramsUI = renderUI({
  pickerInput(
    inputId = "selectedprogram",
    label = NULL,
    choices = programid()
  )
})

#Create a vector of AppIDS that are in the selected Program
programappids = reactive({
  programwbschoice = programwbs()
  programwbschoice = programwbschoice$AppID[which(programwbschoice$ProgramID == input$selectedprogram)]
  return(programwbschoice)
})

#Create a vector of Waterbody Names from the AppIDS
waterbody_names = reactive({
  wbnamesselect = wbnames()
  wbnamesselect = wbnamesselect[which(wbnamesselect$AppID %in% programappids()),]
  wbnamesselectvec = wbnamesselect$AppID
  names(wbnamesselectvec) = wbnamesselect$Waterbody_Name
  return(wbnamesselectvec)
})

#Render the UI for selecting the waterbody
output$selectwbsUI = renderUI({
  validate(
    need(input$selectedprogram,"Loading...")
  )
  
  tagList(
    # textOutput("test"),
    fluidRow(
      column(
        width = 6,
        pickerInput(
          inputId = "selectedwb",
          label = NULL,
          choices = waterbody_names()
        )
      ),
      column(
        width = 6,
        tags$table(
          tags$tr(
            tags$td(
              style = "vertical-align:center; padding-top:6px;",
              HTML("<font size=4><b>"),
              textOutput("selectwbidText"),
              HTML("</font></b>")
            )
          )
        )
      )
    )
  )
})

#Render the waterbodyid text to show next to the selected waterbody name
output$selectwbidText = renderText({
  programwaterbodyidchoice = programwbs()
  programwaterbodyidchoice = programwaterbodyidchoice$ProgramWaterbodyID[which(programwaterbodyidchoice$AppID==input$selectedwb)]
  return(programwaterbodyidchoice)
})

#Render UI for selecting the logger type
output$loggerconfigselectUI = renderUI({
  fluidRow(
    column(
      width = 6,
      pickerInput(
        inputId = "loggerconfigselect",
        label = NULL,
        #logger choices sourced from baseconfigload.R
        choices = loggerchoices
      )
    ),
    column(
      width = 2,
      tags$b("Units")
    ),
    column(
      width = 4,
      uiOutput("loggerunitselectUI")
    )
  )
})

loggerunits = data.frame(
  "Logger_Type" = c("AirBP","AirBP","AirBP","AirBP","AirBP","AirBP","WaterP","WaterP","WaterP","WaterP","WaterP","WaterP","AirTemp",
                    "AirTemp","WaterTemp","WaterTemp","Chlorophylla","Chlorophylla","Chlorophylla","DO","DO","DO","Discharge","Discharge",
                    "GageHeight","GageHeight","GageHeight","GageHeight","Cond","pH","Turbidity"),
  "Unit" = c("psi","Pa","hPa","mm Hg","in Hg","atm","psi","Pa","hPa","mm Hg","in Hg","atm","C","F","C","F","g/cm3","g/L","mg/L","g/cm3",
             "g/L","mg/L","ft3/s","m3/s","ft","in","m","cm","uS/cm","SU","NTU"),
  stringsAsFactors = FALSE
)


loggerunitsconversion = function(qcvalue,Logger_Type,startunit,endunit){
  outputvalue = NA
  if (Logger_Type == "AirBP" | Logger_Type == "WaterP"){
    if (startunit == "psi"){
      if (endunit == "psi"){
        outputvalue = qcvalue
      }else if (endunit == "Pa"){
        outputvalue = qcvalue * 6894.76
      }else if (endunit == "hPa"){
        outputvalue = qcvalue * 68.9476
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue * 51.7149
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue * 2.03602
      }else if (endunit == "atm"){
        outputvalue = qcvalue * 0.068046
      }
    }else if (startunit == "Pa"){
      if (endunit == "psi"){
        outputvalue = qcvalue * 0.000145038
      }else if (endunit == "Pa"){
        outputvalue = qcvalue
      }else if (endunit == "hPa"){
        outputvalue = qcvalue/100 
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue * 0.00750062
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue * 0.0002953
      }else if (endunit == "atm"){
        outputvalue = qcvalue * 9.86923e-6
      }
    }else if (startunit == "hPa"){
      if (endunit == "psi"){
        outputvalue = qcvalue * 0.0145037738
      }else if (endunit == "Pa"){
        outputvalue = qcvalue * 100
      }else if (endunit == "hPa"){
        outputvalue = qcvalue
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue * 0.750062
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue * 0.02953
      }else if (endunit == "atm"){
        outputvalue = qcvalue * 0.000986923
      }
    }else if (startunit == "mm Hg"){
      if (endunit == "psi"){
        outputvalue = qcvalue * 0.0193368
      }else if (endunit == "Pa"){
        outputvalue = qcvalue * 133.322
      }else if (endunit == "hPa"){
        outputvalue = qcvalue * 1.33322
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue * 0.0393701
      }else if (endunit == "atm"){
        outputvalue = qcvalue * 0.00131579
      }
    }else if (startunit == "in Hg"){
      if (endunit == "psi"){
        outputvalue = qcvalue * 0.491154
      }else if (endunit == "Pa"){
        outputvalue = qcvalue * 3386.39
      }else if (endunit == "hPa"){
        outputvalue = qcvalue * 33.8639
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue * 25.4
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue
      }else if (endunit == "atm"){
        outputvalue = qcvalue * 0.0334211
      }
    }else if (startunit == "atm"){
      if (endunit == "psi"){
        outputvalue = qcvalue * 14.6959
      }else if (endunit == "Pa"){
        outputvalue = qcvalue * 101325
      }else if (endunit == "hPa"){
        outputvalue = qcvalue * 1013.25
      }else if (endunit == "mm Hg"){
        outputvalue = qcvalue * 760
      }else if (endunit == "in Hg"){
        outputvalue = qcvalue * 29.9213
      }else if (endunit == "atm"){
        outputvalue = outputvalue
      }
    }
  }else if (Logger_Type == "AirTemp" | Logger_Type == "WaterTemp"){
    if (startunit == "C"){
      if (endunit == "C"){
        outputvalue = qcvalue
      }else if (endunit == "F"){
        outputvalue = (qcvalue * (9/5)) + 32
      }
    }else if (startunit == "F"){
      if (endunit == "C"){
        outputvalue = (qcvalue - 32) * (5/9)
      }else if (endunit == "F"){
        outputvalue = qcvalue
      }
    }
  }else if (Logger_Type == "Chlorophylla" | Logger_Type == "DO"){
    if (startunit == "g/cm3"){
      if (endunit == "g/cm3"){
        outputvalue = qcvalue
      }else if (endunit == "g/L"){
        outputvalue = qcvalue * 1000
      }else if (endunit == "mg/L"){
        outputvalue = qcvalue * 1000000
      }
    }else if (startunit == "g/L"){
      if (endunit == "g/cm3"){
        outputvalue = qcvalue * 0.001
      }else if (endunit == "g/L"){
        outputvalue = qcvalue
      }else if (endunit == "mg/L"){
        outputvalue = qcvalue * 1000
      }
    }else if (startunit == "mg/L"){
      if (endunit == "g/cm3"){
        outputvalue = qcvalue * 1e-6
      }else if (endunit == "g/L"){
        outputvalue = qcvalue * 0.001
      }else if (endunit == "mg/L"){
        outputvalue = qcvalue
      }
    }
  }else if (Logger_Type == "Discharge"){
    if (startunit == "ft3/s"){
      if (endunit == "ft3/s"){
        outputvalue = qcvalue
      }else if (endunit == "m3/s"){
        outputvalue = qcvalue * 0.0283168
      }
    }else if (startunit == "m3/s"){
      if (endunit == "ft3/s"){
        outputvalue = qcvalue * 35.3147
      }else if (endunit == "m3/s"){
        outputvalue =qcvalue
      }
    }
  }else if (Logger_Type == "GageHeight"){
    if (startunit == "ft"){
      if (endunit == "ft"){
        outputvalue = qcvalue
      }else if (endunit == "in"){
        outputvalue = qcvalue * 12
      }else if (endunit == "m"){
        outputvalue = qcvalue * 0.3048
      }else if (endunit == "cm"){
        outputvalue = qcvalue * 30.48
      }
    }else if (startunit == "in"){
      if (endunit == "ft"){
        outputvalue = qcvalue/12
      }else if (endunit == "in"){
        outputvalue = qcvalue
      }else if (endunit == "m"){
        outputvalue = qcvalue * 0.0254
      }else if (endunit == "cm"){
        outputvalue = qcvalue * 0.393701
      }
    }else if (startunit == "m"){
      if (endunit == "ft"){
        outputvalue = qcvalue * 3.28084
      }else if (endunit == "in"){
        outputvalue = qcvalue * 39.3701
      }else if (endunit == "m"){
        outputvalue = qcvalue
      }else if (endunit == "cm"){
        outputvalue = qcvalue * 100
      }
    }else if (startunit == "cm"){
      if (endunit == "ft"){
        outputvalue = qcvalue * 0.0328084
      }else if (endunit == "in"){
        outputvalue = qcvalue * 0.393701
      }else if (endunit == "m"){
        outputvalue = qcvalue * 0.01
      }else if (endunit == "cm"){
        outputvalue = qcvalue
      }
    }
  }else if (Logger_Type == "Cond"){
    if (startunit == "uS/cm"){
      if (endunit == "uS/cm"){
        outputvalue = qcvalue
      }
    }
  }else if (Logger_Type == "pH"){
    if (startunit == "SU"){
      if (endunit == "SU"){
        outputvalue = qcvalue
      }
    }
  }else if (Logger_Type == "Turbidity"){
    if (startunit == "NTU"){
      if (endunit == "NTU"){
        outputvalue = qcvalue
      }
    }
  }
  
  return(outputvalue)
}

#Select Logger Units
output$loggerunitselectUI = renderUI({
  logunitselection = loggerunits$Unit[which(loggerunits$Logger_Type == input$loggerconfigselect)]
  
  qcvalues=wbqcvalues()
  qcunit = unique(qcvalues$Units[which(qcvalues$QC_Metric == "Gross.Fail.Hi" & qcvalues$Logger_Type == input$loggerconfigselect)])
  
  pickerInput(
    inputId = "loggerunitselect",
    label = NULL,
    choices = logunitselection,
    selected = qcunit
  )
})

#RenderUI with QC settings
output$configUI = renderUI({
  validate(
    need(input$loggerconfigselect,"Loading...")
  )
  tagList(
    HTML("<CENTER>"),
    tags$table(
      tags$tr(
        tags$td(
          style = "vertical-align:center; border:1px solid lightgray; padding:10px; background-color:ghostwhite;",
          tags$h4("Gross Range Thresholds"),
          tags$h5("Test if data point exceeds sensor or user defined min/max."),
          HTML("<CENTER>"),
          fluidRow(
            column(
              width = 4,
              tags$h5("Fail")
            ),
            column(
              width = 4,
              tags$h5("Suspect")
            )
          ),
          uiOutput("grossqcUI"),
          HTML("</CENTER>"),
          tags$h4("Spike Thresholds"),
          tags$h5("Test if data point exceeds a user defined threshold relative to the previous data point."),
          HTML("<CENTER>"),
          uiOutput("spikeqcUI"),
          HTML("</CENTER>"),
          HTML("</CENTER>"),
          tags$h4("Rate of Change Limits"),
          tags$h5("Test if a data point exceeds a number of standard deviations from the previous data points over a user defined time period."),
          HTML("<CENTER>"),
          uiOutput("rocqcUI"),
          HTML("</CENTER>"),
          tags$h4("Flat Line Test"),
          tags$h5("Test if a data point is within a user defined threshold from previous data points over a user defined range."),
          HTML("<CENTER>"),
          uiOutput("flatqcUI"),
          HTML("</CENTER>")
        )
      )
    ),
    HTML("</CENTER><br>"),
    fluidRow(
      column(
        width = 4,
        actionBttn(
          inputId = "saveqcbttn",
          label = "Save QC Settings",
          style = "fill",
          color = "success",
          size = "md"
        )
      ),
      column(
        width = 1
      ),
      column(
        width = 4,
        HTML(paste0("<font size=5 color=gray> <p align = left>",savestatus(),"</p></font>"))
      ),
      column(
        width = 3,
        actionBttn(
          inputId = "restoreqcdefaultsbttn",
          label = "Restore Defaults",
          style = "fill",
          color = "primary",
          size = "sm"
        )
      )
    )
  )
})

#Select values for AppID
wbqcvalues = reactive({
  qcvaluesselect = qc_config()
  qcvaluesselect = qcvaluesselect[which(qcvaluesselect$AppID == input$selectedwb),]
  return(qcvaluesselect)
})

#Gross values UI
output$grossqcUI = renderUI({
  validate(
    need(input$loggerconfigselect,"Loading..."),
    need(input$loggerunitselect,"Loading...")
  )
  qcvalues = wbqcvalues()
  qcunit = unique(qcvalues$Units[which(qcvalues$QC_Metric == "Gross.Fail.Hi" & qcvalues$Logger_Type == input$loggerconfigselect)])
  
  #AirBP
  airbpqcvalues=qcvalues[which(qcvalues$Logger_Type == "AirBP"),]
  
  airbpgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airbpgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #AirTemp
  airtempqcvalues=qcvalues[which(qcvalues$Logger_Type == "AirTemp"),]
  airtempgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airtempgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Chlorophyll A
  chlorophyllaqcvalues=qcvalues[which(qcvalues$Logger_Type == "Chlorophylla"),]
  chlorophyllagross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllagrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllagrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllagrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllagrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Conductivity
  condqcvalues=qcvalues[which(qcvalues$Logger_Type == "Cond"),]
  condgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "condgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Discharge
  dischargeqcvalues=qcvalues[which(qcvalues$Logger_Type == "Discharge"),]
  dischargegross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dischargegrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargegrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargegrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargegrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #DO
  doqcvalues=qcvalues[which(qcvalues$Logger_Type == "DO"),]
  dogross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dogrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dogrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dogrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dogrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Gage Height
  gageheightqcvalues=qcvalues[which(qcvalues$Logger_Type == "GageHeight"),]
  gageheightgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "gageheightgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #pH
  phqcvalues=qcvalues[which(qcvalues$Logger_Type == "pH"),]
  phgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "phgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Turbidity
  turbidityqcvalues=qcvalues[which(qcvalues$Logger_Type == "Turbidity"),]
  turbiditygross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "turbiditygrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbiditygrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbiditygrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbiditygrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Water P
  waterpqcvalues=qcvalues[which(qcvalues$Logger_Type == "WaterP"),]
  waterpgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "waterpgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Water Temp
  watertempqcvalues=qcvalues[which(qcvalues$Logger_Type == "WaterTemp"),]
  watertempgross = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "watertempgrossmaxfail",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Gross.Fail.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempgrossminfail",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Gross.Fail.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempgrossmaxsusp",
        label = "Max",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Gross.Suspect.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempgrossminsusp",
        label = "Min",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Gross.Suspect.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  if (input$loggerconfigselect == "AirBP"){
    return(airbpgross)
  }
  else if (input$loggerconfigselect == "AirTemp"){
    return(airtempgross)
  }
  else if (input$loggerconfigselect == "Chlorophylla"){
    return(chlorophyllagross)
  }
  else if (input$loggerconfigselect == "Cond"){
    return(condgross)
  }
  else if (input$loggerconfigselect == "Discharge"){
    return(dischargegross)
  }
  else if (input$loggerconfigselect == "DO"){
    return(dogross)
  }
  else if (input$loggerconfigselect == "GageHeight"){
    return(gageheightgross)
  }
  else if (input$loggerconfigselect == "pH"){
    return(phgross)
  }
  else if (input$loggerconfigselect == "Turbidity"){
    return(turbiditygross)
  }
  else if (input$loggerconfigselect == "WaterP"){
    return(waterpgross)
  }
  else if (input$loggerconfigselect == "WaterTemp"){
    return(watertempgross)
  }
})

#Spike Values UI
output$spikeqcUI = renderUI({
  validate(
    need(input$loggerconfigselect,"Loading..."),
    need(input$loggerunitselect,"Loading...")
  )
  qcvalues=wbqcvalues()
  qcunit = unique(qcvalues$Units[which(qcvalues$QC_Metric == "Spike.Hi" & qcvalues$Logger_Type == input$loggerconfigselect)])
  
  #AirBP
  airbpqcvalues = qcvalues[which(qcvalues$Logger_Type == "AirBP"),]
  airbpspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airbpspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #AirTemp
  airtempqcvalues = qcvalues[which(qcvalues$Logger_Type == "AirTemp"),]
  airtempspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airtempspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Chlorophyll A
  chlorophyllaqcvalues = qcvalues[which(qcvalues$Logger_Type == "Chlorophylla"),]
  chlorophyllaspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllaspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllaspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Conductivity
  condqcvalues = qcvalues[which(qcvalues$Logger_Type == "Cond"),]
  condspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "condspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Discharge
  dischargeqcvalues = qcvalues[which(qcvalues$Logger_Type == "Discharge"),]
  dischargespike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dischargespikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargespikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #DO
  doqcvalues = qcvalues[which(qcvalues$Logger_Type == "DO"),]
  dospike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dospikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dospikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Gage Height
  gageheightqcvalues = qcvalues[which(qcvalues$Logger_Type == "GageHeight"),]
  gageheightspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "gageheightspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #pH
  phqcvalues = qcvalues[which(qcvalues$Logger_Type == "pH"),]
  phspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "phspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #Turbidity
  turbidityqcvalues = qcvalues[which(qcvalues$Logger_Type == "Turbidity"),]
  turbidityspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "turbidityspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbidityspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #WaterP
  waterpqcvalues = qcvalues[which(qcvalues$Logger_Type == "WaterP"),]
  waterpspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "waterpspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  #WaterTemp
  watertempqcvalues = qcvalues[which(qcvalues$Logger_Type == "WaterTemp"),]
  watertempspike = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "watertempspikemaxfail",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Spike.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempspikeminfail",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Spike.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    )
  )
  
  if (input$loggerconfigselect == "AirBP"){
    return(airbpspike)
  }
  else if (input$loggerconfigselect == "AirTemp"){
    return(airtempspike)
  }
  else if (input$loggerconfigselect == "Chlorophylla"){
    return(chlorophyllaspike)
  }
  else if (input$loggerconfigselect == "Cond"){
    return(condspike)
  }
  else if (input$loggerconfigselect == "Discharge"){
    return(dischargespike)
  }
  else if (input$loggerconfigselect == "DO"){
    return(dospike)
  }
  else if (input$loggerconfigselect == "GageHeight"){
    return(gageheightspike)
  }
  else if (input$loggerconfigselect == "pH"){
    return(phspike)
  }
  else if (input$loggerconfigselect == "Turbidity"){
    return(turbidityspike)
  }
  else if (input$loggerconfigselect == "WaterP"){
    return(waterpspike)
  }
  else if (input$loggerconfigselect == "WaterTemp"){
    return(watertempspike)
  }
})

#Rate of Change Values UI
output$rocqcUI = renderUI({
  qcvalues=wbqcvalues()
  
  #AirBP
  airbpqcvalues=qcvalues[which(qcvalues$Logger_Type == "AirBP"),]
  airbproc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airbprocsd",
        label = "SDs",
        width = 100,
        value = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbprocperiod",
        label = "Hours",
        width = 100,
        value = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  
  #AirTemp
  airtempqcvalues=qcvalues[which(qcvalues$Logger_Type == "AirTemp"),]
  airtemproc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airtemprocsd",
        label = "SDs",
        width = 100,
        value = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtemprocperiod",
        label = "Hours",
        width = 100,
        value = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #Chlorophyll A
  chlorophyllaqcvalues=qcvalues[which(qcvalues$Logger_Type == "Chlorophylla"),]
  chlorophyllaroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllarocsd",
        label = "SDs",
        width = 100,
        value = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllarocperiod",
        label = "Hours",
        width = 100,
        value = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #Conductivity
  condqcvalues=qcvalues[which(qcvalues$Logger_Type == "Cond"),]
  condroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "condrocsd",
        label = "SDs",
        width = 100,
        value = condqcvalues$Value[which(condqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condrocperiod",
        label = "Hours",
        width = 100,
        value = condqcvalues$Value[which(condqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #Discharge
  dischargeqcvalues=qcvalues[which(qcvalues$Logger_Type == "Discharge"),]
  dischargeroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dischargerocsd",
        label = "SDs",
        width = 100,
        value = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargerocperiod",
        label = "Hours",
        width = 100,
        value = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #DO
  doqcvalues=qcvalues[which(qcvalues$Logger_Type == "DO"),]
  doroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dorocsd",
        label = "SDs",
        width = 100,
        value = doqcvalues$Value[which(doqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dorocperiod",
        label = "Hours",
        width = 100,
        value = doqcvalues$Value[which(doqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  gageheightqcvalues=qcvalues[which(qcvalues$Logger_Type == "GageHeight"),]
  gageheightroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "gageheightrocsd",
        label = "SDs",
        width = 100,
        value = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightrocperiod",
        label = "Hours",
        width = 100,
        value = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #pH
  phqcvalues=qcvalues[which(qcvalues$Logger_Type == "pH"),]
  phroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "phrocsd",
        label = "SDs",
        width = 100,
        value = phqcvalues$Value[which(phqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phrocperiod",
        label = "Hours",
        width = 100,
        value = phqcvalues$Value[which(phqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #Turbidity
  turbidityqcvalues=qcvalues[which(qcvalues$Logger_Type == "Turbidity"),]
  turbidityroc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "turbidityrocsd",
        label = "SDs",
        width = 100,
        value = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbidityrocperiod",
        label = "Hours",
        width = 100,
        value = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #WaterP
  waterpqcvalues=qcvalues[which(qcvalues$Logger_Type == "WaterP"),]
  waterproc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "waterprocsd",
        label = "SDs",
        width = 100,
        value = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterprocperiod",
        label = "Hours",
        width = 100,
        value = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  #WaterTemp
  watertempqcvalues=qcvalues[which(qcvalues$Logger_Type == "WaterTemp"),]
  watertemproc = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "watertemprocsd",
        label = "SDs",
        width = 100,
        value = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "RoC.SD.number")],
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertemprocperiod",
        label = "Hours",
        width = 100,
        value = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "RoC.SD.period")],
        step = 0.1
      )
    )
  )
  
  if (input$loggerconfigselect == "AirBP"){
    return(airbproc)
  }
  else if (input$loggerconfigselect == "AirTemp"){
    return(airtemproc)
  }
  else if (input$loggerconfigselect == "Chlorophylla"){
    return(chlorophyllaroc)
  }
  else if (input$loggerconfigselect == "Cond"){
    return(condroc)
  }
  else if (input$loggerconfigselect == "Discharge"){
    return(dischargeroc)
  }
  else if (input$loggerconfigselect == "DO"){
    return(doroc)
  }
  else if (input$loggerconfigselect == "GageHeight"){
    return(gageheightroc)
  }
  else if (input$loggerconfigselect == "pH"){
    return(phroc)
  }
  else if (input$loggerconfigselect == "Turbidity"){
    return(turbidityroc)
  }
  else if (input$loggerconfigselect == "WaterP"){
    return(waterproc)
  }
  else if (input$loggerconfigselect == "WaterTemp"){
    return(watertemproc)
  }
})

#Flat Values UI
output$flatqcUI = renderUI({
  validate(
    need(input$loggerconfigselect,"Loading..."),
    need(input$loggerunitselect,"Loading...")
  )
  qcvalues=wbqcvalues()
  qcunit = unique(qcvalues$Units[which(qcvalues$QC_Metric == "Spike.Hi" & qcvalues$Logger_Type == input$loggerconfigselect)])
  
  #AirBP
  airbpqcvalues = qcvalues[which(qcvalues$Logger_Type == "AirBP"),]
  airbpflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airbpflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airbpflattol",
        label = "Tolerance",
        width = 100,
        value = airbpqcvalues$Value[which(airbpqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Air Temp
  airtempqcvalues = qcvalues[which(qcvalues$Logger_Type == "AirTemp"),]
  airtempflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "airtempflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "airtempflattol",
        label = "Tolerance",
        width = 100,
        value = airtempqcvalues$Value[which(airtempqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Chlorophyll A
  chlorophyllaqcvalues = qcvalues[which(qcvalues$Logger_Type == "Chlorophylla"),]
  chlorophyllaflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllaflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllaflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "chlorophyllaflattol",
        label = "Tolerance",
        width = 100,
        value = chlorophyllaqcvalues$Value[which(chlorophyllaqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Conductivity
  condqcvalues = qcvalues[which(qcvalues$Logger_Type == "Cond"),]
  condflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "condflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = condqcvalues$Value[which(condqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "condflattol",
        label = "Tolerance",
        width = 100,
        value = condqcvalues$Value[which(condqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Discharge
  dischargeqcvalues = qcvalues[which(qcvalues$Logger_Type == "Discharge"),]
  dischargeflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "dischargeflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargeflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "dischargeflattol",
        label = "Tolerance",
        width = 100,
        value = dischargeqcvalues$Value[which(dischargeqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #DO
  doqcvalues = qcvalues[which(qcvalues$Logger_Type == "DO"),]
  doflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "doflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "doflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = doqcvalues$Value[which(doqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "doflattol",
        label = "Tolerance",
        width = 100,
        value = doqcvalues$Value[which(doqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Gage Height
  gageheightqcvalues = qcvalues[which(qcvalues$Logger_Type == "GageHeight"),]
  gageheightflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "gageheightflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "gageheightflattol",
        label = "Tolerance",
        width = 100,
        value = gageheightqcvalues$Value[which(gageheightqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #pH
  phqcvalues = qcvalues[which(qcvalues$Logger_Type == "pH"),]
  phflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "phflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = phqcvalues$Value[which(phqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "phflattol",
        label = "Tolerance",
        width = 100,
        value = phqcvalues$Value[which(phqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #Turbidity
  turbidityqcvalues = qcvalues[which(qcvalues$Logger_Type == "Turbidity"),]
  turbidityflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "turbidityflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbidityflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "turbidityflattol",
        label = "Tolerance",
        width = 100,
        value = turbidityqcvalues$Value[which(turbidityqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #WaterP
  waterpqcvalues = qcvalues[which(qcvalues$Logger_Type == "WaterP"),]
  waterpflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "waterpflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "waterpflattol",
        label = "Tolerance",
        width = 100,
        value = waterpqcvalues$Value[which(waterpqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  #WaterTemp
  watertempqcvalues = qcvalues[which(qcvalues$Logger_Type == "WaterTemp"),]
  watertempflat = fluidRow(
    column(
      width = 2,
      numericInput(
        inputId = "watertempflathi",
        label = "Fail",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Flat.Hi")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      ),
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempflatlow",
        label = "Suspect",
        width = 100,
        value = loggerunitsconversion(
          qcvalue = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Flat.Lo")],
          Logger_Type = input$loggerconfigselect,
          startunit = qcunit,
          endunit = input$loggerunitselect
        ),
        step = 0.1
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "watertempflattol",
        label = "Tolerance",
        width = 100,
        value = watertempqcvalues$Value[which(watertempqcvalues$QC_Metric == "Flat.Tolerance")],
        step = 0.1
      )
    )
  )
  
  if (input$loggerconfigselect == "AirBP"){
    return(airbpflat)
  }
  else if (input$loggerconfigselect == "AirTemp"){
    return(airtempflat)
  }
  else if (input$loggerconfigselect == "Chlorophylla"){
    return(chlorophyllaflat)
  }
  else if (input$loggerconfigselect == "Cond"){
    return(condflat)
  }
  else if (input$loggerconfigselect == "Discharge"){
    return(dischargeflat)
  }
  else if (input$loggerconfigselect == "DO"){
    return(doflat)
  }
  else if (input$loggerconfigselect == "GageHeight"){
    return(gageheightflat)
  }
  else if (input$loggerconfigselect == "pH"){
    return(phflat)
  }
  else if (input$loggerconfigselect == "Turbidity"){
    return(turbidityflat)
  }
  else if (input$loggerconfigselect == "WaterP"){
    return(waterpflat)
  }
  else if (input$loggerconfigselect == "WaterTemp"){
    return(watertempflat)
  }
})

#Save changes to QC values
observeEvent(
  input$saveqcbttn,{
    updateqcconfig = qc_config()
    waterbody = input$selectedwb
    
    if (input$loggerconfigselect == "AirBP"){
      logger = "AirBP"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$airbpgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$airbpgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$airbpgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$airbpgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$airbpspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$airbpspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$airbprocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$airbprocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$airbpflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$airbpflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$airbpflattol
    } else if (input$loggerconfigselect == "AirTemp"){
      logger = "AirTemp"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$airtempgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$airtempgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$airtempgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$airtempgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$airtempspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$airtempspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$airtemprocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$airtemprocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$airtempflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$airtempflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$airtempflattol
    } else if (input$loggerconfigselect == "Chlorophylla"){
      logger = "Chlorophylla"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$chlorophyllagrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$chlorophyllagrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$chlorophyllagrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$chlorophyllagrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$chlorophyllaspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$chlorophyllaspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$chlorophyllarocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$chlorophyllarocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$chlorophyllaflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$chlorophyllaflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$chlorophyllaflattol
    } else if (input$loggerconfigselect == "Cond"){
      logger = "Cond"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$condgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$condgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$condgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$condgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$condspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$condspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$condrocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$condrocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$condflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$condflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$condflattol
    }else if (input$loggerconfigselect == "Discharge"){
      logger = "Discharge"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$dischargegrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$dischargegrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$dischargegrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$dischargegrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$dischargespikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$dischargespikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$dischargerocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$dischargerocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$dischargeflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$dischargeflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$dischargeflattol
    }else if (input$loggerconfigselect == "DO"){
      logger = "DO"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$dogrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$dogrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$dogrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$dogrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$dospikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$dospikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$dorocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$dorocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$doflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$doflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$doflattol
    }else if (input$loggerconfigselect == "GageHeight"){
      logger = "GageHeight"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$gageheightgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$gageheightgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$gageheightgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$gageheightgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$gageheightspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$gageheightspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$gageheightrocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$gageheightrocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$gageheightflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$gageheightflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$gageheightflattol
    }else if (input$loggerconfigselect == "pH"){
      logger = "pH"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$phgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$phgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$phgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$phgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$phspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$phspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$phrocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$phrocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$phflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$phflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$phflattol
    }else if (input$loggerconfigselect == "Turbidity"){
      logger = "Turbidity"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$turbiditygrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$turbiditygrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$turbiditygrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$turbiditygrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$turbidityspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$turbidityspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$turbidityrocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$turbidityrocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$turbidityflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$turbidityflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$turbidityflattol
    }else if (input$loggerconfigselect == "WaterP"){
      logger = "WaterP"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$waterpgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$waterpgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$waterpgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$waterpgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$waterpspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$waterpspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$waterprocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$waterprocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$waterpflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$waterpflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$waterpflattol
    }else if (input$loggerconfigselect == "WaterTemp"){
      logger = "WaterTemp"
      #Gross.Fail.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Hi")] = input$watertempgrossmaxfail
      #Gross.Fail.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Fail.Lo")] = input$watertempgrossminfail
      #Gross.Suspect.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Hi")] = input$watertempgrossmaxsusp
      #Gross.Suspect.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Gross.Suspect.Lo")] = input$watertempgrossminsusp
      #Spike.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Hi")] = input$watertempspikemaxfail
      #Spike.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Spike.Lo")] = input$watertempspikeminfail
      #RoC.SD.number
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.number")] = input$watertemprocsd
      #RoC.SD.period
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "RoC.SD.period")] = input$watertemprocperiod
      #Flat.Hi
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Hi")] = input$watertempflathi
      #Flat.Lo
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Lo")] = input$watertempflatlow
      #Flat.Tolerance
      updateqcconfig$Value[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & updateqcconfig$QC_Metric == "Flat.Tolerance")] = input$watertempflattol
    }
    
    updateqcconfig$Units[which(updateqcconfig$AppID == waterbody & updateqcconfig$Logger_Type == logger & !is.na(updateqcconfig$Units))] = input$loggerunitselect
    
    qc_config(updateqcconfig)
    updatebaseconfig()
    savestatus("Saved")
  }
)

#Remove the save status text when the logger type is changed
observeEvent(
  input$loggerconfigselect,{
    savestatus("")
  }
)

#Restore Default Values
observeEvent(
  input$restoreqcdefaultsbttn,{
    restoredefaultsqc=qc_config()
    defaultvalues=restoredefaultsqc[which(restoredefaultsqc$AppID == 1111111111111111),]
    waterbody = input$selectedwb
    
    loggerunits = data.frame(
      "Logger_Type" = c("AirBP","WaterP","AirTemp","WaterTemp","Chlorophylla","DO","Discharge","GageHeight","Cond","pH","Turbidity"),
      "Unit" = c("psi","psi","C","C","g/cm3","g/cm3","ft3/s","ft","uS/cm","SU","NTU"),
      stringsAsFactors = FALSE
    )
    
    selectunit = loggerunits$Unit[which(loggerunits$Logger_Type == input$loggerconfigselect)]
    
    if (input$loggerconfigselect == "AirBP"){
      logger = "AirBP"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "AirTemp"){
      logger = "AirTemp"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "Chlorophylla"){
      logger = "Chlorophylla"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "Cond"){
      logger = "Cond"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "Discharge"){
      logger = "Discharge"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "DO"){
      logger = "DO"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "GageHeight"){
      logger = "GageHeight"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "pH"){
      logger = "pH"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "Turbidity"){
      logger = "Turbidity"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "WaterP"){
      logger = "WaterP"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }else if (input$loggerconfigselect == "WaterTemp"){
      logger = "WaterTemp"
      loggervalues = defaultvalues[which(defaultvalues$Logger_Type == logger),]
      #Gross.Fail.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Hi")]
      #Gross.Fail.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Fail.Lo")]
      #Gross.Suspect.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Suspect.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Hi")]
      #Gross.Suspect.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Gross.Fail.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Gross.Suspect.Lo")]
      #Spike.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Hi")]
      #Spike.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Spike.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Spike.Lo")]
      #RoC.SD.number
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.number")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.number")]
      #RoC.SD.period
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "RoC.SD.period")] = loggervalues$Value[which(loggervalues$QC_Metric == "RoC.SD.period")]
      #Flat.Hi
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Hi")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Hi")]
      #Flat.Lo
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Lo")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Lo")]
      #Flat.Tolerance
      restoredefaultsqc$Value[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & restoredefaultsqc$QC_Metric == "Flat.Tolerance")] = loggervalues$Value[which(loggervalues$QC_Metric == "Flat.Tolerance")]
    }
    
    restoredefaultsqc$Units[which(restoredefaultsqc$AppID == waterbody & restoredefaultsqc$Logger_Type == logger & !is.na(restoredefaultsqc$Units))] = selectunit
    
    qc_config(restoredefaultsqc)
    updatebaseconfig()
    savestatus("Defaults Restored")
  }
)

#QC Config Description UI
output$configdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Edit the QC thresholds and limits of the ContDataQC automatic quality control checks. Select the program, waterbody, and logger type. After changing the values, click the 
    Save QC Settings button. If you want to revert back to the original values, click Restore Defaults button.",
    HTML("</i></font>")
  )
})