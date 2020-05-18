#Vis QC Options----
output$visqcoptions=renderUI({
  VisQCdataoptions=VisQCvalues()
  isolate({siteidchoices=unique(VisQCdataoptions$Serial_Number)})
  isolate(
  tagList(
    pickerInput(
      inputId = "siteidchoice",
      label = "Serial Number",
      choices = siteidchoices,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3")),
    HTML("<CENTER>"),
    uiOutput("DOmetric"),
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
    tags$br(),
    tags$br(),
    actionBttn(
      inputId = "visfail",
      label = "Fail",
      color = "danger",
      style="fill",
      block = TRUE
    ),
    tags$br(),
    actionBttn(
      inputId = "vissusp",
      label = "Suspect",
      color = "warning",
      style = "fill",
      block = TRUE
    ),
    tags$br(),
    actionBttn(
      inputId = "vispass",
      label = "Pass",
      color = "primary",
      style = "fill",
      block = TRUE
    ),
    HTML("</CENTER>")
  )
  )
})

#Display Depth for selected SiteID----
output$depthout=renderText({
  siteiddepthdf=VisQCvalues()
  siteiddepthdf=siteiddepthdf[as.character(siteiddepthdf$Serial_Number) == as.character(input$siteidchoice),]
  siteiddepth=unique(siteiddepthdf$Depth_m)
})

#Metric Type
output$DOmetric=renderUI({
  if (input$Type=="Dissolved Oxygen"){
    radioGroupButtons(
      inputId = "DOmeasure",
      choices = c("DO","Temperature"),
      status = "primary"
    )
  }
})