

output$exportUI = renderUI({
  tagList(
    box(
      title = "Data Structure",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tags$table(
        tags$tr(
          tags$td(
            style = "border-right: 1px solid darkgray; padding-right:5px; width:10%",
            awesomeRadio(
              inputId = "separatefile",
              choices = c("Single File","Separate Files"),
              label = NULL,
              status = "success"
            )
          ),
          tags$td(
            style = "border-right: 3px solid black; padding-left:5px; padding-right:5px; width:10%",
            HTML("Export data in a single file or in multiple files by logger type"),
          ),
          tags$td(
            style = "border-right: 1px solid darkgray; padding-left:5px; padding-right:5px; width:15%",
            materialSwitch(
              inputId = "incmeta",
              label = "Include metadata",
              value = TRUE,
              status = "success"
            )
          ),
          tags$td(
            style = "border-right: 3px solid black; padding-left:5px; padding-right:5px; width:10%",
            HTML("Compile and export metadata"),
          ),
          tags$td(
            style = "border-right: 1px solid darkgray; padding-left:5px; padding-right:5px;",
            materialSwitch(
              inputId = "incconfig",
              label = "Include config file",
              value = TRUE,
              status = "success"
            )
          ),
          tags$td(
            style = "border-right: 3px solid black; padding-left:5px; padding-right:5px; width:10%",
            HTML("Export the configuration file holding all of the app data. Can be used the next time the app is used."),
          ),
          tags$td(
            style = "padding-left:5px; padding-right:5px;",
            materialSwitch(
              inputId = "increp",
              label = "Include report",
              value = TRUE,
              status = "success"
            )
          )
        )
      )
    )
  )
})

#Download Data----
output$download <- downloadHandler(
  filename = function() { 
    if (input$Type=="Temperature"){
      paste0(lakeinput(),"_Temperature_",gsub("-","_",as.character(Sys.Date())),".csv")
    }else if (input$Type=="Dissolved Oxygen"){
      paste0(lakeinput(),"_DO_",gsub("-","_",as.character(Sys.Date())),".csv")
    }
  },
  content = function(file) {
    dataforexport=exportdata()
    write.csv(dataforexport,file,row.names=F)
  })
