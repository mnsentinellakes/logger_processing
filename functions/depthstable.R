##!!!Potential Future Update - Read the data files in first to collect list of serial numbers and then associate depths with each
fieldnames = reactive({
  fieldnameselect = export()
  fieldnameselect = fieldnameselect[which(fieldnameselect$ProgramID == input$procprogram &  fieldnameselect$ModelID == input$procmodel),]
  return(fieldnameselect)
})


#A column of delete buttons for each row in the data frame for the first column
deleteButtonColumn = function(df, id, fieldnames, ...) {
  
  if (nrow(fieldnames) > 0){
    if (fieldnames$IncZ == TRUE & !is.na(fieldnames$UnitID)){
      unitidfieldname = as.character(fieldnames$UnitID)
      zfieldname = as.character(fieldnames$Z)
      colnamessettings = c("UnitID","Z")
      names(colnamessettings) = c(unitidfieldname,zfieldname)
      notvisible = list(visible = FALSE,targets = c(3:7))
    }else if (fieldnames$IncZ == TRUE & is.na(fieldnames$UnitID)){
      
      unitidfieldname = "UnitID"
      zfieldname = as.character(fieldnames$Z)
      colnamessettings = c("UnitID","Z")
      
      names(colnamessettings) = c(unitidfieldname,zfieldname)
      notvisible = list(visible = FALSE,targets = c(3:7))
    }else if (fieldnames$IncZ == FALSE & !is.na(fieldnames$UnitID)){
      unitidfieldname = as.character(fieldnames$UnitID)
      colnamessettings = c("UnitID")
      names(colnamessettings) = unitidfieldname
      
      notvisible = list(visible = FALSE,targets = c(2:7))
    }else if (fieldnames$IncZ == FALSE & is.na(fieldnames$UnitID)){
      colnamessettings = c("UnitID" = "UnitID")
      notvisible = list(visible = FALSE,targets = c(2:7))
    }
  }else{
    colnamessettings = c("UnitID" = "UnitID","Z" = "Z")
    notvisible = list(visible = FALSE,targets = c(3:7))
  }
  
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
                        notvisible),
      # dom = 't',
      scrollY = "250px",
      searching = FALSE,
      paging = FALSE
    ),
    colnames = colnamessettings
  )
}

# Extracts the row id number from the id string
parseDeleteEvent = function(idstr) {
  res = as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

output$depthstableoutputUI = renderUI({
  validate(
    need(!is.null(fieldnames()),"Loading...")
  )
  
  if (nrow(fieldnames()) > 0){
    selectedinput = fieldnames()
    
    if (!is.na(selectedinput$UnitID)){
      unitidlabel = selectedinput$UnitID
    }else{
      unitidlabel = "Unit ID"
    }
    
    if (selectedinput$IncZ == TRUE){
      col1 = 4
      col2 = 4
      col3 = 3
    }else{
      col1 = 4
      col2 = 1
      col3 = 4
    }
    
    box(
      title = "Logger Units Table",
      solidHeader = TRUE,
      collapsible = FALSE,
      status = "primary",
      width = NULL,
      tags$table(
        tags$tr(
          tags$td(
            style="vertical-align:center; border:1px solid lightgray; padding:5px; background-color:ghostwhite;",
            fluidRow(
              column(
                width = col1,
                #Serial Number Text Input
                textInput(
                  inputId = "inputsn",
                  label = unitidlabel
                )
              ),
              column(
                width = col2,
                uiOutput("zoptionUI")
              ),
              column(
                width = col3,
                #Add serial number and depth data to lookup table
                tags$br(),
                actionBttn(
                  inputId = "addbttn",
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
  }else if (nrow(fieldnames()) == 0){
    box(
      title = "Logger Model not configured for this Program",
      solidHeader = TRUE,
      status = "danger",
      HTML("To process data for the selected Logger Model, the settings under the Export Options need to be configured."),
      width = NULL
    )
  }
})

output$zoptionUI = renderUI({
  
  if (nrow(fieldnames()) > 0){
    # validate(
    #   need(fieldnames(),"Loading...")
    # )
    zfields = fieldnames()
    
    if (nrow(zfields) > 0){
      zvalue = zfields$Z
    }else{
      zvalue = NA
    }
    
    if (zfields$IncZ == TRUE){
      #Depth Text Input
      textInput(
        inputId = "inputdepth",
        label = zvalue
      )
    }else{}
  }
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
  # validate(
  #   need(fieldnames(),"Loading...")
  # )  
  
  deleteButtonColumn(
    df = depthstablefilter(),
    id = "delete_button",
    fieldnames = fieldnames()
  )
})

#In-table delete
observeEvent(
  input$deletePressed,{
    rowNum = parseDeleteEvent(input$deletePressed)
    allproclogs = processinglogs()
    depthstableselect = depthstablefilter()
    depthstabledel = depthstableselect$ProcID[rowNum]
    
    allproclogs = allproclogs[which(allproclogs$ProcID != depthstabledel),]
    
    processinglogs(allproclogs)
    
    updatebaseconfig()
  }
)

observeEvent(
  input$addbttn,{
    if (!is.null(input$procmodel)){
      if (nchar(input$inputsn) > 0 & nchar(input$inputdepth) > 0){
        addproclogs = processinglogs()
        
        addproclogsrow = data.frame("UnitID" = input$inputsn,"Z" = input$inputdepth,"Processed" = NA,
                                    "ModelID" = input$procmodel,"StationID" = input$procstationname,"DeployID" = NA,
                                    "ProcID" = random_id(n=1,bytes = 12),stringsAsFactors = FALSE)
        
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
        addproclogsrow = data.frame("UnitID" = input$inputsn,"Z" = NA,"Processed" = NA,
                                    "ModelID" = input$procmodel,"StationID" = input$procstationname,"DeployID" = NA,
                                    "ProcID" = random_id(n=1,bytes = 12),stringsAsFactors = FALSE)
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
  }
)
