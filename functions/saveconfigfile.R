#UI for saving the config file
output$saveconfigUI = renderUI({
  tagList(
    textInput(
      inputId = "saveconfigname",
      label = NULL,
      placeholder = "Configuration File Name"
    ),
    downloadBttn(
      outputId = "saveconfigbttn",
      label = "Save Configuration",
      style = "fill"
    )
  )
})

#Config file download
output$saveconfigbttn = downloadHandler(
  filename = function() {
    paste0(input$saveconfigname,".RData")
  },
  content = function(file){
    load("config/baseconfig.RData")
    save(baseconfig,file = file)
  }
)

#UI Description
output$saveconfigdescUI = renderUI({
  tags$p(
    HTML("<font size = 4><i>"),
    "The configuration file contains all of the program, waterbodies, qc, and logger model settings, as well as logger processing history 
    and metadata. Save this file after making any changes to these settings.",
    HTML("</i></font>")
  )
})