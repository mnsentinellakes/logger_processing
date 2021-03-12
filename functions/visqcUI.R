#Vis QC Options----

output$visqcoptionsUI = renderUI({
  isolate(
    tagList(
      uiOutput("visqcdatatypesUI"),
      
      #Place in own UI
      uiOutput("unitidchoiceUI"),
      uiOutput("depthoutUI"),
      HTML("</CENTER><br><h5><b>QC Flag Type</b></h5><CENTER>"),
      pickerInput(
        inputId = "flagselect",
        choices = c("Visual","Gross","Spike","Rate of Change","Flat"),
        selected = "Visual"
      ),
      tags$br(),
      HTML("</CENTER><h5><b>Manual Visual QC Flags</b></h5><CENTER>"),
      actionBttn(
        inputId = "failbttn",
        label = "Fail",
        color = "danger",
        style = "fill",
        block = TRUE
      ),
      tags$br(),
      actionBttn(
        inputId = "suspbttn",
        label = "Suspect",
        color = "warning",
        style = "fill",
        block = TRUE
      ),
      tags$br(),
      actionBttn(
        inputId = "passbttn",
        label = "Pass",
        color = "primary",
        style = "fill",
        block = TRUE
      ),
      HTML("</CENTER>")
    )
  )
})

#Picker input to select data logger type
output$visqcdatatypesUI = renderUI({
  isolate(
    pickerInput(
      inputId = "visqcloggerchoices",
      choices = qcloggertypes(),
      label = "Data Type"
    )
  )
})

#select data frame for chosen data type
datatypedf = reactive({
  validate(
    need(input$visqcloggerchoices,"Loading...")
  )
  VisQCdataoptions = VisQCdata()
  datatypeselect = VisQCdataoptions[[input$visqcloggerchoices]]
  
  return(datatypeselect)
})

selectedsn = reactiveVal(NULL)

#Picker input to select serial number/unitid
output$unitidchoiceUI = renderUI({
  validate(
    need(datatypedf(),"Loading..."),
    need(nrow(fieldnames()) > 0,"Loading...")
  )
  
  unitidname = fieldnames()
  if (!is.na(unitidname$UnitID)){
    unitidnamechoice = unitidname$UnitID
  }else{
    unitidnamechoice = "Unit ID"
  }
  
  isolate({
    unitidchoices = datatypedf()
    unitidchoices = unique(unitidchoices$UnitID)
    if(is.null(selectedsn())){
      selection = unitidchoices[1]
    }else{
      selection = selectedsn()
    }
    
    pickerInput(
      inputId = "unitidchoice",
      label = unitidnamechoice,
      choices = unitidchoices,
      selected = selection,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"))
  })
})

#Update selectedsn
observe({
  selectedsn(input$unitidchoice)
})

#Display Depth for selected unitID----
output$depthoutUI = renderUI({
  validate(
    need(datatypedf(),"Loading..."),
    need(input$unitidchoice,"Loading..."),
    need(nrow(fieldnames()) > 0,"Loading...")
  )
  unitidzdf = datatypedf()
  unitidz = unique(unitidzdf$Z[which(unitidzdf$UnitID == input$unitidchoice)])
  zname = fieldnames()
  
  tagList(
    HTML(paste0("<H5><B>",zname$Z,"</B></H5>")),
    HTML("<CENTER>"),
    fluidRow(
      column(
        width = 5
      ),
      column(
        width = 2,
        tags$table(
          tags$tr(
            tags$td(
              style = "border: 1px solid black; padding: 5px; padding-top: 2px; padding-bottom: 2px;",
              HTML("<font size=5>",unitidz,"</font>")
            )
          )
        )
      ),
      column(
        width = 5
      )
    )
  )
})

output$visqcdescUI = renderUI({
  
  tags$p(
    HTML("<font size = 4><i>"),
    "These tools allow the user to interactively select and flag data points for QC. The QC Tools choices can be used to cycle through the 
    logger data types, unit ids, and qc flags. There is an additional toolbar located at the top right corner of the main plot. These tools 
    can be used to zoom and pan around the plot as well as select the data points. Once data points are selected, they can be flagged using
    the QC Flag buttons.",
    HTML("</i></font>")
  )
})