#Load lake metadata
lakeid=reactive({
  lakemeta=mnsentinellakes::sentinellakesmetadata
  lakeidselect=lakemeta$LakeId[which(formatlakename(lakemeta$Lake)==lakeinput())]
  return(lakeidselect)
})

#Load depthdata file
load("data/depthdata")

#Create reactive value table for the depths data
depthsfile = reactiveVal()

#Select which depthdata dataframe to use depending on input choice
observe(
  if (input$Type=="Temperature"){
    depthsfile(depthdata[[1]])
    
  }else if (input$Type=="Dissolved Oxygen"){
    depthsfile(depthdata[[2]])
  }
)

#Format Lake Name
formatlakename=function(lakename){
  lakename=gsub(" ","_",lakename)
  lakename=gsub("[:.:]","",lakename)
  return(lakename)
}

#Create event reactive lake name---------------
lakeinput=eventReactive(
  input$lakeapply,{
    isolate(as.character(formatlakename(input$lakeinput)))
  }) 

#observes the add button on the setup page, when pressed it adds the values in the Serial Number and Depth text inputs into the lookup table
observeEvent(
  input$add,{
    if (nchar(lakeinput())>0){
      if (nchar(input$inputsn)>0){
        Rowcalc=depthsfile()
        RID=as.numeric(Rowcalc[nrow(Rowcalc),1])+1
        lakename=as.character(formatlakename(lakeinput()))
        WB_ID=lakeid()
        SN=as.character(input$inputsn)
        print(SN)
        if (!is.na(input$inputdepth)){
          depthnew=input$inputdepth
        }else{
          depthnew=NA
        }
        
        newrow=data.frame("RowID"=RID,"Lake"=lakename,"LakeId"=WB_ID,"Serial_Number"=SN,"Depth"=depthnew,"Processed"=NA)
        Rowcalc=rbind(Rowcalc,newrow)
        depthsfile(Rowcalc)
      }else{}
    }else{}
  })

#observes the add button on the setup page, when pressed it adds the values in the Serial Number and Depth text inputs into the lookup table
#Delete Rows-----------
#Observes the Delete button and deletes the row indicated in the text input
observeEvent(
  input$delete,{
    dftest=depthsfile()
    # dftest=dftest[dftest$Lake==lakeinput(),]
    if (nchar(input$RowID)>0 & input$RowID %in% dftest$RowID){
      rownum=as.numeric(input$RowID)
      depthsfile(dftest[!(dftest$RowID==rownum),])
    }
  })

observeEvent(
  input$reset,{
    resettest=depthsfile()
    
    if (nchar(input$RowID)>0 & input$RowID %in% resettest$RowID){
      resetrownum=as.numeric(input$RowID)
      resettest$Processed[which(resettest$RowID==resetrownum)]=NA
      depthsfile(resettest)
    }
  })

#Observes the add button and clears text inputs after a value has been entered
observeEvent(
  input$add,{
    updateTextInput(
      session,
      "inputsn",
      value = NA
    )
    updateTextInput(
      session,
      inputId = "inputdepth",
      value = NA)
    updateTextInput(
      session,
      inputId = "startdate",
      value = NA
    )
    updateTextInput(
      session,
      inputId = "enddate",
      value = NA
    )
  })

#Observes the Delete button and clears text inputs after a value has been entered
observeEvent(
  input$delete,{
    updateTextInput(
      session,
      inputId = "RowID",
      value = NA
    )
  })

#Observes the Reset button and clears text inputs after a value has been entered
observeEvent(
  input$reset,{
    updateTextInput(
      session,
      inputId = "RowID",
      value=NA
    )
  })

#Depths Table UI Display
output$depthstableoutput=renderDT(
  rownames=FALSE,{
    datadepths=depthsfile()
    
    if (input$lakeapply==0){
      datadepths
    }else{
      datadepths=datadepths[datadepths$Lake==lakeinput(),]
    }
    datadepths
  })

#Updates the Processed date in the depthsfile when the "Process Data" button is clicked
observeEvent(
  input$processing,{
    updatedates=depthsfile()
    updatedates$Processed[which(is.na(updatedates$Processed))]=Sys.Date()
    if(input$Type=="Temperature"){
      depthdata[[1]]=updatedates
    }else if(input$Type=="Dissolved Oxygen"){
      depthdata[[2]]=updatedates
    }
    save(depthdata,file = "data/depthdata")
  })