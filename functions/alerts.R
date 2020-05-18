#Data Entry Alerts----
#Missing Depth Input Alert
#Observes the add button. If the Depth text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$add,{
    if (nchar(input$inputdepth)==0){
      sendSweetAlert(
        session,
        title = "Missing Depth",
        text = "Ignore if Depth has been purposely omitted",
        type = "warning"
      )
    }
  })

#Observes the add button. If the Serial Number text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$add,{
    if (nchar(input$inputsn)==0){
      sendSweetAlert(
        session,
        title = "Missing Serial Number",
        text = "",
        type = "error"
      )
    }
  })

#Observes the add button. If the lake name text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$add,{
    if(nchar(input$lakeinput)==0){
      sendSweetAlert(
        session,
        title = "Missing Lake Name",
        text = "",
        type = "error"
      )
    }
  })

#Data Editing Alerts----
#Missing RowID
#Observes the Delete button If the row id text input is blank, an alert will popup, telling the user to fill out the input
observeEvent(
  input$delete,{
    if (nchar(input$RowID)==0){
      sendSweetAlert(
        session,
        title = "Missing RowID",
        text = "",
        type = "error"
      )
    }
  })