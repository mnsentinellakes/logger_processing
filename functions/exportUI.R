#Primary UI for export
output$exportUI = renderUI({
  fluidRow(
    column(
      width = 9,
      box(
        title = "Export Data",
        solidHeader = TRUE,
        width = NULL,
        status = "primary",
        uiOutput("exportprocesstoolsUI"),
        tags$br(),
        box(
          title = "Data Preview",
          collapsible = TRUE,
          collapsed = FALSE,
          width = NULL,
          status = "success",
          DTOutput("finaltable")
        ),
        tags$br(),
        uiOutput("dlddataUI")
      )
    ),
    column(
      width = 3,
      box(
        solidHeader = TRUE,
        status = "success",
        width = NULL,
        title = "",
        uiOutput("exportdescUI")
      )
    )
  )
})

#UI for button and progress bar
output$exportprocesstoolsUI = renderUI({
  fluidRow(
    column(
      width = 4,
      tags$br(),
      uiOutput("exportprocessbuttonUI")
    ),
    column(
      width = 5,
      progressBar(
        "exportprogress",
        value = 0,
        display_pct = TRUE,
        status = "success",
        title = "Begin Processing"
      )
    ),
    column(
      width = 3,
      uiOutput("exportviewUI")
    )
  )
})

#UI for export process button
output$exportprocessbuttonUI = renderUI({
  if (!is.null(VisQCdata())){
    actionBttn(
      inputId = "exportprocessbttn",
      label = "Process Data for Export",
      style = "fill",
      color = "success",
      size = "lg"
    )
  }
})

#UI for download bttn
output$dlddataUI = renderUI({
  if (!is.null(finaldata())){
    fluidRow(
      column(
        width = 4,
        downloadBttn(
          outputId = "dlddata",
          label = "Download Data",
          style = "material-flat",
          size = "lg"
        )
      ),
      column(
        width = 6,
        progressBar(
          "dldprogress",
          value = 0,
          display_pct = TRUE,
          status = "success",
          title = ""
        )
      )
    )
  }
})


#Options for selecting the logger type if data are processed into separate files
output$exportviewUI = renderUI({
  if (!is.null(finaldata())){
    if (is.data.frame(finaldata())){
    }else{
      pickerInput(
        inputId = "exportviewloggertype",
        label = "Logger Types",
        #qcloggertypes() sourced from processing.R
        choices = names(finaldata())
      )
    }
  }else{}
})

#Data Preview Table
output$finaltable = renderDT(
  options = list(
    lengthChange = FALSE,
    searching = FALSE,
    pageLength = 5
    # scrollY = "250px",
    # paging = FALSE
  ),
  rownames = FALSE,
  extensions = 'Responsive',{
    
    if (!is.null(finaldata())){
      finaldatatable = finaldata()
      
      if (is.data.frame(finaldatatable)){
        return(finaldatatable)
      }else{
        validate(
          need(input$exportviewloggertype,"Loading...")
        )
        finaldatatableselect = finaldatatable[[input$exportviewloggertype]]
        return(finaldatatableselect)
      }
    }else{}
  }
)

#Description UI
output$exportdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "Click the Process Data for Export button to organize the data into its final state. The data can be previewed in the Data Preview box.
    If the data formatting looks correct, click on the Download Data button to begin the download process.",
    HTML("</i></font>")
  )
})