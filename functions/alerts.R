#Data Entry Alerts----
#Missing Depth Input Alert
#Observes the add button. If the Depth text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$addbttn,{
    includez = fieldnames()
    includez = includez$IncZ
    if (includez == TRUE){
      if (nchar(input$inputdepth)==0){
        sendSweetAlert(
          session,
          title = "Missing Z Value",
          text = "Ignore if Z Value has been purposely omitted",
          type = "warning"
        )
      }
    }
  }
)

#Observes the add button. If the Serial Number text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$addbttn,{
    if (nchar(input$inputsn)==0){
      sendSweetAlert(
        session,
        title = "Missing Unit ID",
        text = "",
        type = "error"
      )
    }
  }
)