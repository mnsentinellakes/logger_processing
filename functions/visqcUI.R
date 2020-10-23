#Vis QC Options----

visqcloggertypes = reactive({
  visqcloggers=VisQCdata()
  visqcloggers = names(visqcloggers)
  return(visqcloggers)
})

output$visqcoptions=renderUI({
  
  isolate(
    tagList(
      uiOutput("visqcdatatypesUI"),
      
      #Place in own UI
      uiOutput("siteidchoiceUI"),
      HTML("<CENTER>"),
      HTML("</CENTER>"),
      HTML("<H5><B>Depth</B></H5>"),
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
                style="border: 1px solid black; padding: 5px; padding-top: 2px; padding-bottom: 2px;",
                HTML("<font size=5>"),
                textOutput("depthout"),
                HTML("</font>")
              )
            )
          )
        ),
        column(
          width = 5
        )
      ),
      HTML("</CENTER><br><h5><b>QC Flag Type</b></h5><CENTER>"),
      pickerInput(
        inputId = "flagselect",
        choices = c("Visual","Gross","Spike","Rate of Change","Flat"),
        selected = "Visual"
      ),
      tags$br(),
      HTML("</CENTER><h5><b>Manual Visual QC Flags</b></h5><CENTER>"),
      actionBttn(
        inputId = "fail",
        label = "Fail",
        color = "danger",
        style="fill",
        block = TRUE
      ),
      tags$br(),
      actionBttn(
        inputId = "susp",
        label = "Suspect",
        color = "warning",
        style = "fill",
        block = TRUE
      ),
      tags$br(),
      actionBttn(
        inputId = "pass",
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
  pickerInput(
    inputId = "visqcloggerchoices",
    choices = visqcloggertypes(),
    label = "Data Type"
  )
})

#select data frame for chosen data type
datatypedf = reactive({
  validate(
    need(input$visqcloggerchoices,"Loading...")
  )
  
  VisQCdataoptions=VisQCdata()
  datatypeselect = VisQCdataoptions[[input$visqcloggerchoices]]
  return(datatypeselect)
  
})

#Picker input to select serial number/siteid
output$siteidchoiceUI = renderUI({
  validate(
    need(datatypedf(),"Loading...")
  )
  
  siteidchoices = datatypedf()
  siteidchoices = unique(siteidchoices$SiteId)
  
  pickerInput(
    inputId = "siteidchoice",
    label = "Serial Number",
    choices = siteidchoices,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"))
})

#Display Depth for selected SiteID----
output$depthout = renderText({
  validate(
    need(datatypedf(),"Loading..."),
    need(input$siteidchoice,"Loading...")
  )
  siteiddepthdf = datatypedf()
  siteiddepth = unique(siteiddepthdf$Depth[which(siteiddepthdf$SiteId == input$siteidchoice)])
  return(siteiddepth)
})