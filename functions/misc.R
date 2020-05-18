#Creates UI for holding Logger String text input box if the logger type is == Temperature
output$loggerstring=renderUI({
  if (input$Type=="Temperature"){
    textInput(
      inputId = "chain",
      label = "Logger Chain",
      value = "Primary"
    )
  }else{}
})