##!!!Potential Future Update - Read the data files in first to collect list of serial numbers and then associate depths with each


#A column of delete buttons for each row in the data frame for the first column
deleteButtonColumn = function(df, id, ...) {
  # function to create one action button as string
  f = function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  deleteCol = unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(
    cbind(df,Delete = deleteCol),
    # Need to disable escaping for html as string to work
    escape = FALSE,
    options = list(
      # Disable sorting for the delete column
      columnDefs = list(list(targets = 1, sortable = FALSE),
                        list(visible = FALSE, targets = c(3:7))),
      # dom = 't',
      scrollY = "250px",
      searching = FALSE,
      paging = FALSE
    ),
    colnames = c("Serial Number" = "Serial_Number","Depth" = "Depth")
  )
}

# Extracts the row id number from the id string
parseDeleteEvent = function(idstr) {
  res = as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

output$depthstableoutputUI = renderUI({
  box(
    title = "Depths Table",
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    width = NULL,
    tags$table(
      tags$tr(
        tags$td(
          style="vertical-align:center; border:1px solid lightgray; padding:5px; background-color:ghostwhite;",
          fluidRow(
            column(
              width = 4,
              #Serial Number Text Input
              textInput(
                inputId = "inputsn",
                label = "Serial Number"
              )
            ),
            column(
              width = 4,
              #Depth Text Input
              textInput(
                inputId = "inputdepth",
                label = "Depth"
              )
            ),
            column(
              width = 3,
              #Add serial number and depth data to lookup table
              tags$br(),
              actionBttn(
                inputId = "add",
                label = "Add",
                color = "success",
                style = "fill",
                icon = icon("plus")
              )
            )
          )
        )
      )
    ),
    tags$br(),
    HTML("<CENTER>"),
    #Table output for lookup table
    DTOutput("depthstableoutput"),
    HTML("</CENTER>"),
    textOutput("testx")
  )
})

depthstablefilter = reactive({
  validate(
    need(input$procwaterbody,"Loading...")
  )
  proclogs = processinglogs()
  
  proclogs = proclogs[which(proclogs$StationID == input$procstationname & proclogs$ModelID == input$procmodel & is.na(proclogs$Processed)),]
  
  
  return(proclogs)
})

#Depths Table UI Display
output$depthstableoutput = renderDataTable({
    
  deleteButtonColumn(depthstablefilter(),"delete_button")
  
  })

#In-table delete
observeEvent(input$deletePressed, {
  rowNum = parseDeleteEvent(input$deletePressed)
  allproclogs = processinglogs()
  depthstableselect = depthstablefilter()
  depthstabledel = depthstableselect$ProcID[rowNum]
  
  allproclogs = allproclogs[which(allproclogs$ProcID != depthstabledel),]

  processinglogs(allproclogs)
  
  updatebaseconfig()
  
})

observeEvent(
  input$add,
  {
    if (!is.null(input$procmodel)){
      if (nchar(input$inputsn) > 0 & nchar(input$inputdepth) > 0){
        
        addproclogs = processinglogs()
        
        addproclogsrow = data.frame("Serial_Number" = input$inputsn,"Depth" = input$inputdepth,"Processed" = NA,
                                    "ModelID" = input$procmodel,"StationID" = input$procstationname,"DeployID" = NA,"ProcID" = random_id(n=1,bytes = 12),
                                    stringsAsFactors = FALSE)
        
        addproclogs = rbind(addproclogs,addproclogsrow)
        
        processinglogs(addproclogs)
        
        updatebaseconfig()
        
        updateTextInput(
          session = session,
          inputId = "inputsn",
          value = ""
        )
        
        updateNumericInput(
          session = session,
          inputId = "inputdepth",
          value = ""
        )
        
      }else if (nchar(input$inputsn) == 0 & nchar(input$inputdepth) > 0){
        
      }else if (nchar(input$inputsn) > 0 & nchar(input$inputdepth) == 0){
        
        addproclogs = processinglogs()
        addproclogsrow = data.frame("Serial_Number" = input$inputsn,"Depth" = NA,"Processed" = NA,
                                    "ModelID" = input$procmodel,"StationID" = input$procstationname,"DeployID" = NA,"ProcID" = random_id(n=1,bytes = 12),
                                    stringsAsFactors = FALSE)
        addproclogs = rbind(addproclogs,addproclogsrow)
        
        processinglogs(addproclogs)
        updatebaseconfig()
        
        updateTextInput(
          session = session,
          inputId = "inputsn",
          value = ""
        )
        
        updateNumericInput(
          session = session,
          inputId = "inputdepth",
          value = ""
        )
      }
    }else{
      sendSweetAlert(
        session = session,
        title = "Missing Logger Model",
        text = "A Logger Model must be created and selected for this logger type before processing.",
        type = "error"
      )
    }
  })

#Updates the Processed date in the depthsfile when the "Process Data" button is clicked
# observeEvent(
#   input$processing,
#   {
#     updatedates = processinglogs()
#     updatedates$Processed[which(updatedates$StationID == input$procstationname &
#                                   updatedates$ModelID == input$procmodel & is.na(updatedates$Processed))] = Sys.Date()
#     processinglogs(updatedates)
#     updatebaseconfig()
#   })