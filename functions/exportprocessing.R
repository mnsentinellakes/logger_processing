#Reactive value to store data for export
selectexportdata = reactiveVal()

#Update the deployment table dates
deploydatesupdate = function(){
  #datatypedf() from visqcUI code file
  deployqcdata = datatypedf()
  deployupdate = deploylogs()
  
  minrecorddt = unique(as.character(min(deployqcdata$DateTime)))
  
  maxrecorddt = unique(as.character(max(deployqcdata$DateTime)))
  minvaliddt = unique(as.character(min(deployqcdata$DateTime[which(deployqcdata$FlagVis == 'P')])))
  maxvaliddt = unique(as.character(max(deployqcdata$DateTime[which(deployqcdata$FlagVis == 'P')])))
  
  deployupdate$StartDateTimeRecord[which(deployupdate$DeployID == deployid())] = minrecorddt
  deployupdate$EndDateTimeRecord[which(deployupdate$DeployID == deployid())] = maxrecorddt
  deployupdate$StartDateTimeValid[which(deployupdate$DeployID == deployid())] = minvaliddt
  deployupdate$EndDateTimeValid[which(deployupdate$DeployID == deployid())] = maxvaliddt
  
  deploylogs(deployupdate)
  
  updatebaseconfig()
}

#Select the relevant row from the export table
observeEvent(
  input$exportprocessbttn,{
    exportdata = export()
    #input$procprogram and input$procmodel are sourced from the processingUI.R file
    exportdata = exportdata[which(exportdata$ProgramID == input$procprogram & exportdata$ModelID == input$procmodel),]
    selectexportdata(exportdata)
  }
)

#function for updating the export progress bar
updateexportprogress = function(sumprog,metaprog,repprog,progsection,allparams = NULL,selectedparam = NULL){
  if (!is.null(sumprog) & !is.null(metaprog) & !is.null(repprog)){
    
    if (sumprog == TRUE & metaprog == TRUE & repprog == TRUE){
      procitems = message(paste0("---All exports"))
      dpv = 30
      spv = 15
      mpv = 10
      rpv = 45
    }else if (sumprog == TRUE & metaprog == TRUE & repprog == FALSE){
      procitems = message(paste0("---No Report"))
      dpv = 65
      spv = 20
      mpv = 15
      rpv = 0
    }else if (sumprog == TRUE & metaprog == FALSE & repprog == TRUE){
      procitems = message(paste0("---No Metadata"))
      dpv = 35
      spv = 15
      mpv = 0
      rpv = 50
    }else if (sumprog == TRUE & metaprog == FALSE & repprog == FALSE){
      procitems = message(paste0("---No Metadata and Report"))
      dpv = 80
      spv = 20
      mpv = 0
      rpv = 0
    }else if (sumprog == FALSE & metaprog == TRUE & repprog == TRUE){
      procitems = message(paste0("---No Summary"))
      dpv = 35
      spv = 0
      mpv = 15
      rpv = 50
    }else if (sumprog == FALSE & metaprog == TRUE & repprog == FALSE){
      procitems = message(paste0("---No Summary and Report"))
      dpv = 75
      spv = 0
      mpv = 25
      rpv = 0
    }else if (sumprog == FALSE & metaprog == FALSE & repprog == TRUE){
      procitems = message(paste0("---No Summary and Metadata"))
      dpv = 40
      spv = 0
      mpv = 0
      rpv = 60
    }else if (sumprog == FALSE & metaprog == FALSE & repprog == FALSE){
      procitems = message(paste0("---Data Only"))
      dpv = 100
      spv = 0
      mpv = 0
      rpv = 0
    }
    
    if (progsection == "Data1"){
      pv = dpv * 0.1
      txt = "Beginning data processing"
    }else if (progsection == "Data2"){
      pv = dpv * 0.2
      txt = "Processing dates and times"
    }else if (progsection == "Data3"){
      dpvi = (dpv - (dpv * 0.2)) / length(allparams)
      pv = (dpv * 0.2) + (dpvi * match(selectedparam,allparams))
      txt = paste("Processing ",selectedparam," data")
    }else if (progsection == "Data4"){
      pv = dpv
      txt = "Data processing complete"
    }else if (progsection %in% c("Sum1","Sum2","Sum3")){
      if (progsection == "Sum1"){
        pv = dpv + (spv *0.1)
        txt = "Beginning data summary"
      }else if (progsection == "Sum2"){
        spvi = (spv - (spv * 0.2)) / length(allparams)
        pv = dpv + ((spv * 0.2) + (spvi * match(selectedparam,allparams)))
        txt = paste("Summarizing ",selectedparam," data")
      }else if (progsection == "Sum3"){
        pv = dpv + spv
        txt = "Data summarization complete"
      }
    }else if (progsection %in% c("Meta1","Meta2")){
      if (progsection == "Meta1"){
        pv = dpv + spv + (mpv * 0.5)
        txt = "Beginning metadata compilation"
      }else if (progsection == "Meta2"){
        pv = dpv + spv + mpv
        txt = "Metadata compilation complete"
      }
    }else if (progsection %in% c("Report1","Report2","Report3","Report4")){
      if (progsection == "Report1"){
        pv = dpv + spv + mpv + (rpv * 0.1)
        txt = "Beginning building report"
      }else if (progsection == "Report2"){
        pv = dpv + spv + mpv + (rpv * 0.2)
        txt = "Defining report parameters"
      }else if (progsection == "Report3"){
        pv = dpv + spv + mpv + (rpv * 0.3)
        txt = "Compiling report (This may take a minute)"
      }else if (progsection == "Report4"){
        pv = dpv + spv + mpv + rpv
        txt = "Report completed"
      }
    }
    message(progsection)
    message(paste0("-",pv))
    message(paste0("--",txt))
    procitems
    
    updateProgressBar(
      id = "exportprogress",
      session = session,
      value = pv,
      status = "success",
      title = txt
    )
  }
}


#Process data for export
finaldata = reactiveVal()
finaldatacomplete = reactiveVal(FALSE)
observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    
    validate(
      need(selectexportdata(),"Loading...")
    )
    message("finaldataprocessing")
    deploydatesupdate()
    #Export Data Settings
    exportsettings = selectexportdata()
    
    updateexportprogress(
      sumprog = exportsettings$IncSum,
      metaprog = exportsettings$IncMeta,
      repprog = exportsettings$IncRep,
      progsection = "Data1"
    )
    
    #Program Names
    programsettings = programs()
    #Waterbodies
    programwbssettings = programwbs()
    wbnamessettings = wbnames()
    #Stations
    stationssettings = stations()
    #Deploy Logs
    deploylogssettings = deploylogs()
    
    #LoggerFiles
    loggerfilesettings = loggerfiledefs()
    #Export Data
    processdata = VisQCdata()
    
    #Select the first data frame to begin building the final table
    startfields = processdata[[1]]
    
    #Select the UnitId and DateTime Fields
    startfields = startfields[,c(1:2)]
    
    updateexportprogress(
      sumprog = exportsettings$IncSum,
      metaprog = exportsettings$IncMeta,
      repprog = exportsettings$IncRep,
      progsection = "Data2"
    )
    
    #Rename UnitID Field
    names(startfields)[1] = exportsettings$UnitID
    #Format Date Fields
    #Set TZ
    startfields[,2] = with_tz(
      time = startfields[,2],
      tzone = exportsettings$TZ
    )
    
    #Split or separate dates
    if (exportsettings$DateTimeSep == "Combined"){
      names(startfields)[2] = exportsettings$Date_Time
    }else if (exportsettings$DateTimeSep == "Separate"){
      startfields$date = as.Date(startfields[,2])
      names(startfields)[3] = exportsettings$Date
      startfields$time = format(startfields[,2], format = "%H:%M:%S")
      names(startfields)[4] = exportsettings$Time
      startfields[,2] = NULL
    }
    
    #Add Model Name
    if (exportsettings$IncModelName == TRUE){
      startfields$modname = storeloggermodel()
      names(startfields)[ncol(startfields)] = exportsettings$ModelName
    }
    
    #Add Program Name
    if (exportsettings$IncProgramName == TRUE){
      startfields$progname = storeprogram()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramName
    }
    
    #Add WaterBody ID
    if (exportsettings$IncProgramWBID == TRUE){
      startfields$wbid = storewbid()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramWBID
    }
    
    #Add WaterBody Name
    if (exportsettings$IncWBName == TRUE){
      startfields$wbname = storewbname()
      names(startfields)[ncol(startfields)] = exportsettings$WBName
    }
    
    #Add WaterBody Type
    if (exportsettings$IncWBType == TRUE){
      #input$procwaterbody sourced from processingUI.R
      startfields$wbtype = wbnamessettings$programwbssettings$WB_Type[which(programwbssettings$AppID == input$procwaterbody)]
      names(startfields)[ncol(startfields)] = exportsettings$WBType
    }
    
    #Add Station ID
    if (exportsettings$IncProgramStationID == TRUE){
      startfields$stationid = storestationid()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramStationID
    }
    
    #Add Station Name
    if (exportsettings$IncStationName == TRUE){
      startfields$stationname = storestationname()
      names(startfields)[ncol(startfields)] = exportsettings$StationName
    }
    
    #Add Coordinates
    if (exportsettings$IncLoc == TRUE){
      startfields$lat = storelat()
      names(startfields)[ncol(startfields)] = exportsettings$Lat
      startfields$lon = storelon()
      names(startfields)[ncol(startfields)] = exportsettings$Lon
    }
    
    #Add Deployment Count
    if (exportsettings$IncDeploy == TRUE){
      startfields$deploy = storedepcount()
      names(startfields)[ncol(startfields)] = exportsettings$Deployment
    }
    
    #Add User Name
    if (exportsettings$IncUser == TRUE){
      startfields$user = storeuser()
      names(startfields)[ncol(startfields)] = exportsettings$User
    }
    
    if (exportsettings$FileSep == "Single"){
      combinefields = startfields
      #qcloggertypes() sourced from processing.R
      for (i in qcloggertypes()){
        
        updateexportprogress(
          sumprog = exportsettings$IncSum,
          metaprog = exportsettings$IncMeta,
          repprog = exportsettings$IncRep,
          progsection = "Data3",
          allparams = qcloggertypes(),
          selectedparam = i
        )
        
        #Select parameter Data and Depth Fields and rename them
        paradata = processdata[[i]]
        paradata = paradata[,c(3,4)]
        
        unitidname = as.character(dplyr::select(exportsettings,matches(i)))
        
        names(paradata)[1] = unitidname
        if (exportsettings$IncZ == TRUE){
          names(paradata)[2] = exportsettings$Z
        }else if (exportsettings$IncZ == FALSE){
          paradata[,2] = NULL
        }
        
        #Select Flag fields and rename them
        qcdata = processdata[[i]]
        
        if (exportsettings$IncNotes == TRUE){
          qcdata = qcdata[,c(15:20)]
        }else{
          qcdata = qcdata[,c(15:19)]
        }
        
        if (length(qcloggertypes()) > 1){
          names(qcdata) = paste0(i,"_",names(qcdata))
        }
        
        #Combine parameter data and qc data
        loggercombine = cbind(paradata,qcdata)
        
        #Combine logger types
        combinefields = cbind(combinefields,loggercombine)
        
      }
      
      #Create vector for sorting fields in the table
      #Start with UnitID (required)
      fieldorder = c(exportsettings$UnitID)
      #Model Name
      if (exportsettings$IncModelName == TRUE){
        fieldorder = c(fieldorder,exportsettings$ModelName)
      }
      
      #Program Name
      if (exportsettings$IncProgramName == TRUE){
        fieldorder = c(fieldorder,exportsettings$ProgramName)
      }
      #Waterbody ID
      if (exportsettings$IncProgramWBID == TRUE){
        fieldorder = c(fieldorder,exportsettings$ProgramWBID)
      }
      #Waterbody Name
      if (exportsettings$IncWBName == TRUE){
        fieldorder = c(fieldorder,exportsettings$WBName)
      }
      #Waterbody Type
      if (exportsettings$IncWBType == TRUE){
        fieldorder = c(fieldorder,exportsettings$WBType)
      }
      #Station ID
      if (exportsettings$IncProgramStationID == TRUE){
        fieldorder = c(fieldorder,exportsettings$ProgramStationID)
      }
      #Station Name
      if (exportsettings$IncStationName == TRUE){
        fieldorder = c(fieldorder,exportsettings$StationName)
      }
      #Location
      if(exportsettings$IncLoc == TRUE){
        fieldorder = c(fieldorder,exportsettings$Lat,exportsettings$Lon)
      }
      #Deployment
      if (exportsettings$IncDeploy == TRUE){
        fieldorder = c(fieldorder,exportsettings$Deployment)
      }
      #User Name
      if (exportsettings$IncUser == TRUE){
        fieldorder = c(fieldorder,exportsettings$User)
      }
      #Z
      if (exportsettings$IncZ == TRUE){
        fieldorder = c(fieldorder,exportsettings$Z)
      }
      
      #Add Date and Time
      if (exportsettings$DateTimeSep == "Combined"){
        fieldorder = c(fieldorder,exportsettings$Date_Time)
      }else{
        fieldorder = c(fieldorder,exportsettings$Date,exportsettings$Time)
      }
      #Add Parameters
      for (i in qcloggertypes()){
        parametertype = as.character(dplyr::select(exportsettings,matches(i)))
        fieldorder = c(fieldorder,parametertype)
      }
      #Add QC
      if (length(qcloggertypes()) > 1){
        for (i in qcloggertypes()){
          
          if (exportsettings$IncNotes == TRUE){
            qcflags = c("FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis","Notes")
          }else{
            qcflags = c("FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
          }
          qcflags = paste0(i,"_",qcflags)
          fieldorder = c(fieldorder,qcflags)
        }
      }else{
        if (exportsettings$IncNotes == TRUE){
          fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis","Notes")
        }else{
          fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
        }
      }
      
      combinefields = combinefields[,fieldorder]
      finaldata(combinefields)
    }else if (exportsettings$FileSep == "Multiple"){
      combinelist = list()
      for (i in qcloggertypes()){
        
        updateexportprogress(
          sumprog = exportsettings$IncSum,
          metaprog = exportsettings$IncMeta,
          repprog = exportsettings$IncRep,
          progsection = "Data3",
          allparams = qcloggertypes(),
          selectedparam = i
        )
        
        #Select data by logger type
        paradata = processdata[[i]]
        
        #Remove extra QC Flag Fields
        paradata = paradata[,c(3,4)]
        names(paradata)[1] = dplyr::select(exportsettings,matches(i))
        if (exportsettings$IncZ == TRUE){
          names(paradata)[2] = exportsettings$Z
        }else if (exportsettings$IncZ == FALSE){
          paradata[,2] = NULL
        }
        
        #Select Flag fields
        qcdata = processdata[[i]]
        
        if (exportsettings$IncNotes == TRUE) {
          qcdata = qcdata[,c(15:20)]
        }else{
          qcdata = qcdata[,c(15:19)]
        }
        
        paradata = cbind(startfields,paradata)
        combinedata = cbind(paradata,qcdata)
        
        fieldorder = c(exportsettings$UnitID)
        
        #Model Name
        if (exportsettings$IncModelName == TRUE){
          fieldorder = c(fieldorder,exportsettings$ModelName)
        }
        #Program Name
        if (exportsettings$IncProgramName == TRUE){
          fieldorder = c(fieldorder,exportsettings$ProgramName)
        }
        #Waterbody ID
        if (exportsettings$IncProgramWBID == TRUE){
          fieldorder = c(fieldorder,exportsettings$ProgramWBID)
        }
        #Waterbody Name
        if (exportsettings$IncWBName == TRUE){
          fieldorder = c(fieldorder,exportsettings$WBName)
        }
        #Waterbody Type
        if (exportsettings$IncWBType == TRUE){
          fieldorder = c(fieldorder,exportsettings$WBType)
        }
        #Station ID
        if (exportsettings$IncProgramStationID == TRUE){
          fieldorder = c(fieldorder,exportsettings$ProgramStationID)
        }
        #Station Name
        if (exportsettings$IncStationName == TRUE){
          fieldorder = c(fieldorder,exportsettings$StationName)
        }
        if (exportsettings$IncLoc == TRUE){
          fieldorder = c(fieldorder,exportsettings$Lat,exportsettings$Lon)
        }
        #Deployment
        if (exportsettings$IncDeploy == TRUE){
          fieldorder = c(fieldorder,exportsettings$Deployment)
        }
        #User Name
        if (exportsettings$IncUser == TRUE){
          fieldorder = c(fieldorder,exportsettings$User)
        }
        #Z
        if (exportsettings$IncZ == TRUE){
          fieldorder = c(fieldorder,exportsettings$Z)
        }
        #Add Date and Time
        if (exportsettings$DateTimeSep == "Combined"){
          fieldorder = c(fieldorder,exportsettings$Date_Time)
        }else{
          fieldorder = c(fieldorder,exportsettings$Date,exportsettings$Time)
        }
        #Add Parameters
        datafieldname = as.character(dplyr::select(exportsettings,matches(i)))
        fieldorder = c(fieldorder,datafieldname)
        
        #Add QC
        if (exportsettings$IncNotes == TRUE){
          fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis","Notes")
        }else{
          fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
        }
        combinedata = combinedata[,fieldorder]
        
        combinelist[[i]] = combinedata
        
      }
      finaldata(combinelist)
      
    }
    finaldatacomplete(TRUE)
    
    updateexportprogress(
      sumprog = exportsettings$IncSum,
      metaprog = exportsettings$IncMeta,
      repprog = exportsettings$IncRep,
      progsection = "Data4"
    )
    
    #Clear VisQCdata
    if (exportsettings$IncSum == FALSE){
      VisQCdata(NULL)
    }
  }
)

#Compile Daily Summary Data
summarydata = reactiveVal()
summarydatacomplete = reactiveVal(FALSE)
observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    validate(
      need(finaldatacomplete() == TRUE,"Loading..."),
      need(selectexportdata(),"Loading...")
    )
    message("summarydataprocessing")
    sumexportsettings = selectexportdata()
    if (sumexportsettings$IncSum == TRUE){
      
      updateexportprogress(
        sumprog = sumexportsettings$IncSum,
        metaprog = sumexportsettings$IncMeta,
        repprog = sumexportsettings$IncRep,
        progsection = "Sum1"
      )
      
      deploydatesupdate()
      
      #Program Names
      sumprogramsettings = programs()
      #Waterbodies
      sumprogramwbssettings = programwbs()
      sumwbnamessettings = wbnames()
      #Stations
      sumstationssettings = stations()
      #Deploy Logs
      sumdeploylogssettings = deploylogs()
      #LoggerFiles
      sumloggerfilesettings = loggerfiledefs()
      
      origdata = VisQCdata()
      
      #Summarize Data
      sumdata = NULL
      for (i in qcloggertypes()){
        
        updateexportprogress(
          sumprog = sumexportsettings$IncSum,
          metaprog = sumexportsettings$IncMeta,
          repprog = sumexportsettings$IncRep,
          progsection = "Sum2",
          allparams = qcloggertypes(),
          selectedparam = i
        )
        
        origdataselect = origdata[[i]]
        origdataselect = origdataselect[which(origdataselect$FlagVis != 'F'),]
        origdataselect = origdataselect[,1:4]
        origdataselect$DateTime = with_tz(origdataselect$DateTime,tzone = sumexportsettings$TZ)
        origz = unique(origdataselect$Z)
        
        sumdataz = NULL
        for (j in origz){
          if (is.na(j)){
            origzselect = origdataselect
          }else{
            origzselect = origdataselect[which(origdataselect$Z == j),]
          }
          origzunitid = unique(origzselect$UnitID)
          for (k in origzunitid){
            origunit = origzselect[which(origzselect$UnitID == k),]
            
            sumdataz.xts = as.xts(as.numeric(as.numeric(origunit$Data)),order.by = origunit$DateTime)
            sumdataz.mean.xts = apply.daily(sumdataz.xts,mean)
            sumdataz.max.xts = apply.daily(sumdataz.xts,max)
            sumdataz.min.xts = apply.daily(sumdataz.xts,min)
            sumdataz.sd.xts = apply.daily(sumdataz.xts,sd)
            sumdataz.count.xts = apply.daily(sumdataz.xts,nrow)
            
            sumdatazmean = data.frame("unitid" = k,"datetime" = index(sumdataz.mean.xts),"z" = j,"mean" = coredata(sumdataz.mean.xts))
            sumdatazmax = data.frame("unitid" = k,"datetime" = index(sumdataz.max.xts),"z" = j,"max" = coredata(sumdataz.max.xts))
            sumdatazmin = data.frame("unitid" = k,"datetime" = index(sumdataz.min.xts),"z" = j,"min" = coredata(sumdataz.min.xts))
            sumdatazsd = data.frame("unitid" = k,"datetime" = index(sumdataz.sd.xts),"z" = j,"sd" = coredata(sumdataz.sd.xts))
            sumdatazcount = data.frame("unitid" = k,"datetime" = index(sumdataz.count.xts),"z" = j,
                                       "count" = coredata(sumdataz.count.xts))
            sumdatazday = left_join(sumdatazmean,sumdatazmax,by = c("unitid","datetime","z"))
            sumdatazday = left_join(sumdatazday,sumdatazmin,by = c("unitid","datetime","z"))
            sumdatazday = left_join(sumdatazday,sumdatazsd,by = c("unitid","datetime","z"))
            sumdatazday = left_join(sumdatazday,sumdatazcount,by = c("unitid","datetime","z"))
            
            sumdataz = rbind(sumdataz,sumdatazday)
          }
        }
        sumdata[[i]] = sumdataz
      }
      
      #Select the first data frame to begin building the final table
      sumstartfields = sumdata[[1]]
      
      #Convert datetime to date
      sumstartfields$datetime = as.Date(sumstartfields$datetime)
      
      #Select the UnitId and DateTime Fields
      sumstartfields = sumstartfields[,c(1:2)]
      
      
      #Rename UnitID Field
      names(sumstartfields)[1] = sumexportsettings$UnitID
      
      #Rename Date Fields
      names(sumstartfields)[2] = "Date"
      
      #Add Model Name
      if (sumexportsettings$IncModelName == TRUE){
        sumstartfields$summodname = storeloggermodel()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ModelName
      }
      
      #Add Program Name
      if (sumexportsettings$IncProgramName == TRUE){
        sumstartfields$progname = storeprogram()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramName
      }
      
      #Add WaterBody ID
      if (sumexportsettings$IncProgramWBID == TRUE){
        sumstartfields$wbid = storewbid()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramWBID
      }
      
      #Add WaterBody Name
      if (sumexportsettings$IncWBName == TRUE){
        sumstartfields$wbname = storewbname()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$WBName
      }
      
      #Add WaterBody Type
      if (sumexportsettings$IncWBType == TRUE){
        sumstartfields$wbtype = sumwbnamessettings$programwbssettings$WB_Type[which(sumprogramwbssettings$AppID == input$procwaterbody)]
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$WBType
      }
      
      #Add Station ID
      if (sumexportsettings$IncProgramStationID == TRUE){
        sumstartfields$stationid = storestationid()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramStationID
      }
      
      #Add Station Name
      if (sumexportsettings$IncStationName == TRUE){
        sumstartfields$stationname = storestationname()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$StationName
      }
      
      #Add Location
      if (sumexportsettings$IncLoc == TRUE){
        sumstartfields$lat = storelat()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$Lat
        sumstartfields$lon = storelon()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$Lon
      }
      
      #Add Deployment Count
      if (sumexportsettings$IncDeploy == TRUE){
        sumstartfields$deploy = storedepcount()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$Deployment
      }
      
      #Add User Name
      if (sumexportsettings$IncUser == TRUE){
        sumstartfields$user = storeuser()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$User
      }
      
      if (sumexportsettings$FileSep == "Single"){
        #qcloggertypes() sourced from processing.R
        sumcombinefields = sumstartfields
        for (i in qcloggertypes()){
          #Select parameter Data and Depth Fields and rename them
          sumparadata = sumdata[[i]]
          
          sumparadata = sumparadata[,c(3:8)]
          sumunitidname = as.character(dplyr::select(sumexportsettings,matches(i)))
          if (sumexportsettings$IncZ == TRUE){
            names(sumparadata)[1] = sumexportsettings$Z
            names(sumparadata)[2] = paste0(sumunitidname,"_mean")
            names(sumparadata)[3] = paste0(sumunitidname,"_max")
            names(sumparadata)[4] = paste0(sumunitidname,"_min")
            names(sumparadata)[5] = paste0(sumunitidname,"_sd")
            names(sumparadata)[6] = "Count"
          }else if (sumexportsettings$IncZ == FALSE){
            sumparadata[,1] = NULL
            names(sumparadata)[1] = paste0(sumunitidname,"_mean")
            names(sumparadata)[2] = paste0(sumunitidname,"_max")
            names(sumparadata)[3] = paste0(sumunitidname,"_min")
            names(sumparadata)[4] = paste0(sumunitidname,"_sd")
            names(sumparadata)[5] = "Count"
            
          }
          
          #Combine logger types
          sumcombinefields = cbind(sumcombinefields,sumparadata)
        }
        
        #Create vector for sorting fields in the table
        #Start with UnitID (required)
        sumfieldorder = c(sumexportsettings$UnitID)
        #Model Name
        if (sumexportsettings$IncModelName == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$ModelName)
        }
        
        #Program Name
        if (sumexportsettings$IncProgramName == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramName)
        }
        #Waterbody ID
        if (sumexportsettings$IncProgramWBID == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramWBID)
        }
        #Waterbody Name
        if (sumexportsettings$IncWBName == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$WBName)
        }
        #Waterbody Type
        if (sumexportsettings$IncWBType == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$WBType)
        }
        #Station ID
        if (sumexportsettings$IncProgramStationID == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramStationID)
        }
        #Station Name
        if (sumexportsettings$IncStationName == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$StationName)
        }
        if (sumexportsettings$IncLoc == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$Lat,sumexportsettings$Lon)
        }
        #Deployment
        if (sumexportsettings$IncDeploy == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$Deployment)
        }
        #User Name
        if (sumexportsettings$IncUser == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$User)
        }
        #Z
        if (sumexportsettings$IncZ == TRUE){
          sumfieldorder = c(sumfieldorder,sumexportsettings$Z)
        }
        #Add Date and Time
        sumfieldorder = c(sumfieldorder,"Date")
        
        #Add Parameters
        for (i in qcloggertypes()){
          sumparametertype = as.character(dplyr::select(sumexportsettings,matches(i)))
          
          sumfieldorder = c(sumfieldorder,paste0(sumparametertype,"_mean"),paste0(sumparametertype,"_max"),paste0(sumparametertype,"_min"),
                            paste0(sumparametertype,"_sd"),"Count")
        }
        
        sumcombinefields = sumcombinefields[,sumfieldorder]
        summarydata(sumcombinefields)
        
      }else if (sumexportsettings$FileSep == "Multiple"){
        
        sumcombinefields = sumstartfields
        sumcombinelist = list()
        sumcombinedata = NULL
        for (i in qcloggertypes()){
          
          #Select parameter Data and Depth Fields and rename them
          sumparadata = sumdata[[i]]
          sumparadata = sumparadata[,c(3:8)]
          
          sumunitidname = as.character(dplyr::select(sumexportsettings,matches(i)))
          
          if (sumexportsettings$IncZ == TRUE){
            names(sumparadata)[1] = sumexportsettings$Z
            names(sumparadata)[2] = paste0(sumunitidname,"_mean")
            names(sumparadata)[3] = paste0(sumunitidname,"_max")
            names(sumparadata)[4] = paste0(sumunitidname,"_min")
            names(sumparadata)[5] = paste0(sumunitidname,"_sd")
            names(sumparadata)[6] = "Count"
          }else if (sumexportsettings$IncZ == FALSE){
            sumparadata[,1] = NULL
            names(sumparadata)[1] = paste0(sumunitidname,"_mean")
            names(sumparadata)[2] = paste0(sumunitidname,"_max")
            names(sumparadata)[3] = paste0(sumunitidname,"_min")
            names(sumparadata)[4] = paste0(sumunitidname,"_sd")
            names(sumparadata)[5] = "Count"
          }
          
          sumfieldorder = c(sumexportsettings$UnitID)
          sumcombinedata = cbind(sumcombinefields,sumparadata)
          #Model Name
          if (sumexportsettings$IncModelName == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$ModelName)
          }
          #Program Name
          if (sumexportsettings$IncProgramName == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramName)
          }
          #Waterbody ID
          if (sumexportsettings$IncProgramWBID == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramWBID)
          }
          #Waterbody Name
          if (sumexportsettings$IncWBName == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$WBName)
          }
          #Waterbody Type
          if (sumexportsettings$IncWBType == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$WBType)
          }
          #Station ID
          if (sumexportsettings$IncProgramStationID == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$ProgramStationID)
          }
          #Station Name
          if (sumexportsettings$IncStationName == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$StationName)
          }
          #Location
          if (sumexportsettings$IncLoc == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$Lat,sumexporsettings$Lon)
          }
          #Deployment
          if (sumexportsettings$IncDeploy == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$Deployment)
          }
          #User Name
          if (sumexportsettings$IncUser == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$User)
          }
          #Z
          if (sumexportsettings$IncZ == TRUE){
            sumfieldorder = c(sumfieldorder,sumexportsettings$Z)
          }
          
          #Add Parameters
          sumdatafieldname = as.character(dplyr::select(sumexportsettings,matches(i)))
          
          sumfieldorder = c(sumfieldorder,paste0(sumdatafieldname,"_mean"),paste0(sumdatafieldname,"_max"),paste0(sumdatafieldname,"_min"),
                            paste0(sumdatafieldname,"_sd"),"Count")
          
          sumcombinedata = sumcombinedata[,sumfieldorder]
          
          sumcombinelist[[i]] = sumcombinedata
        }
        summarydata(sumcombinelist)
      }
      
      updateexportprogress(
        sumprog = sumexportsettings$IncSum,
        metaprog = sumexportsettings$IncMeta,
        repprog = sumexportsettings$IncRep,
        progsection = "Sum3"
      )
      VisQCdata(NULL)
    }
    summarydatacomplete(TRUE)
  }
)

#Compile Metadata
metadatafinal = reactiveVal()
metadatafinalcomplete = reactiveVal(FALSE)
qcsettingsfinal = reactiveVal()
observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    
    validate(
      need(input$procprogram,"Loading..."),
      need(input$procwaterbody,"Loading..."),
      need(input$procstationname,"Loading..."),
      need(input$procmodel,"Loading..."),
      need(finaldatacomplete() == TRUE,"Loading..."),
      need(summarydatacomplete() == TRUE,"Loading...")
    )
    message("metadatafinalprocessing")
    
    metaexportsettings = selectexportdata()
    
    updateexportprogress(
      sumprog = metaexportsettings$IncSum,
      metaprog = metaexportsettings$IncMeta,
      repprog = metaexportsettings$IncRep,
      progsection = "Meta1"
    )
    
    compiledmetadata = buildmeta(
      programid = input$procprogram,
      appid = input$procwaterbody,
      stationid = input$procstationname,
      deployid = deployid(),
      modelid = input$procmodel,
      programs = programs(),
      programwbs = programwbs(),
      wbnames = wbnames(),
      stations = stations(),
      logfiledefs = loggerfiledefs(),
      deploylogs = deploylogs(),
      qcconfig = qcconfig(),
      export = export()
    )
    metadatafinal(compiledmetadata)
    
    compileqcsettings =  getqcsettings(
      appid = input$procwaterbody,
      stationid = input$procstationname,
      deployid = deployid(),
      deploylogs = deploylogs(),
      qcconfig = qc_config(),
      programwbs = programwbs(),
      wbnames = wbnames(),
      stations = stations()
    )
    qcsettingsfinal(compileqcsettings)
    
    updateexportprogress(
      sumprog = metaexportsettings$IncSum,
      metaprog = metaexportsettings$IncMeta,
      repprog = metaexportsettings$IncRep,
      progsection = "Meta2"
    )
    
    metadatafinalcomplete(TRUE)
  }
)

# Compile Report
observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    validate(
      need(finaldatacomplete() == TRUE,"Loading..."),
      need(summarydatacomplete() == TRUE,"Loading..."),
      need(metadatafinalcomplete() == TRUE,"Loading..."),
      need(selectexportdata(),"Loading...")
    )
    
    message("reportprocessing")
    repexportsettings = selectexportdata()
    if (repexportsettings$IncRep == TRUE){
      
      updateexportprogress(
        sumprog = repexportsettings$IncSum,
        metaprog = repexportsettings$IncMeta,
        repprog = repexportsettings$IncRep,
        progsection = "Report1"
      )
      #Delete Temp folder
      unlink(paste0(getwd(),"/temp"),recursive = TRUE)
      
      dldname = storewbname()
      dldnamestart = paste0(dldname)
      dldnamestart = gsub("\\s+","_",gsub("[[:punct:]]","",dldnamestart))
      
      #Create Temp folder
      dir.create(paste0(getwd(),"/temp"))
      
      updateexportprogress(
        sumprog = repexportsettings$IncSum,
        metaprog = repexportsettings$IncMeta,
        repprog = repexportsettings$IncRep,
        progsection = "Report2"
      )
      
      repstation = stations()
      repstation = repstation[which(repstation$StationID == input$procstationname),]
      
      params = list(
        inputdata = finaldata(),
        metadata = metadatafinal(),
        qcsettings = qcsettingsfinal(),
        export = selectexportdata(),
        stations = repstation,
        app_version = baseconfigversion
      )
      
      reportfile = paste0(getwd(),"/temp/",dldnamestart,"_report.html")
      
      updateexportprogress(
        sumprog = repexportsettings$IncSum,
        metaprog = repexportsettings$IncMeta,
        repprog = repexportsettings$IncRep,
        progsection = "Report3"
      )
      
      rmarkdown::render(
        input = "markdown/report.Rmd",
        output_file = reportfile,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      updateexportprogress(
        sumprog = repexportsettings$IncSum,
        metaprog = repexportsettings$IncMeta,
        repprog = repexportsettings$IncRep,
        progsection = "Report4"
      )
    }
  }
)


#Process and Send Evaluation Data

#Configure Download File
output$dlddata = downloadHandler(
  filename = function() {
    #Name output
    wbnamedldzip = wbnames()
    dldzip = wbnamedldzip$Waterbody_Name[which(wbnamedldzip$AppID == input$procwaterbody)]
    dldzip = paste0(gsub("\\s+","_",gsub("[[:punct:]]","",dldzip)),"_",gsub("-","_",Sys.Date()),".zip")
    return(dldzip)
  },
  content = function(fname){
    exportdldsettings = selectexportdata()
    
    if (exportdldsettings$IncRep == FALSE){
      #Delete Temp folder
      unlink(paste0(getwd(),"/temp"),recursive = TRUE)
      
      #Create Temp folder
      dir.create("temp")
    }
    
    #Get Waterbody Name
    wbnamedld = wbnames()
    dldname = storewbname()
    dldnamestart = paste0(dldname)
    dldnamestart = gsub("\\s+","_",gsub("[[:punct:]]","",dldnamestart))
    
    
    #Process Data for Single File
    if (exportdldsettings$FileSep == "Single"){
      updateProgressBar(
        id = "dldprogress",
        session = session,
        value = 40,
        status = "success",
        title = "Packaging data"
      )
      
      dldnameprime = paste0(dldnamestart,".csv")
      
      dldpathprime = paste0("temp/",dldnameprime)
      
      #Create the csv and zip it
      write.csv(finaldata(),dldpathprime,row.names = FALSE)
      
      zip(
        zipfile = fname,
        files = dldpathprime,
        include_directories = FALSE,  
        recurse = FALSE,
        mode = "cherry-pick"
      )
      
      if (exportdldsettings$IncSum == TRUE){
        updateProgressBar(
          id = "dldprogress",
          session = session,
          value = 60,
          status = "success",
          title = "Packaging summary data"
        )
        
        dldnamesum = paste0(dldnamestart,"_summary.csv")
        dldpathsum = paste0("temp/",dldnamesum)
        
        write.csv(summarydata(),dldpathsum,row.names = FALSE)
        
        zip_append(
          zipfile = fname,
          files = dldpathsum,
          include_directories = FALSE,
          recurse = FALSE,
          mode = "cherry-pick"
        )
      }
      
    }else{
      finaldataout = finaldata()
      #Get a list of logger types from the final data
      listnames = names(finaldataout)
      updateProgressBar(
        id = "dldprogress",
        session = session,
        value = 40,
        status = "success",
        title = "Packaging data"
      )
      #List containing file paths
      listoutput = NULL
      for (i in listnames){
        selectfinaldata = finaldataout[[i]]
        dldname1 = paste0(dldnamestart,"_",i,".csv")
        dldpath = paste0("temp/",dldname1)
        
        #Write csv
        write.csv(selectfinaldata,dldpath,row.names = FALSE)
        listoutput = c(listoutput,dldpath)
      }
      
      #Zip files
      zip(
        zipfile = fname,
        files = listoutput,
        recurse = FALSE,
        mode = "cherry-pick"
      )
      
      if (exportdldsettings$IncSum == TRUE){
        updateProgressBar(
          id = "dldprogress",
          session = session,
          value = 60,
          status = "success",
          title = "Packaging summary data"
        )
        summarydataout = summarydata()
        sumlistnames = names(summarydataout)
        
        sumlistoutput = NULL
        for (i in sumlistnames){
          selectsumdata = summarydataout[[i]]
          
          sumdldnameend = paste0(dldnamestart,"_",i,"_summary.csv")
          sumdldpath = paste0("temp/",sumdldnameend)
          
          write.csv(selectsumdata,sumdldpath,row.names = FALSE)
          sumlistoutput = c(sumlistoutput,sumdldpath)
        }
        
        zip_append(
          zipfile = fname,
          files = sumlistoutput,
          include_directories = FALSE,
          recurse = FALSE,
          mode = "cherry-pick"
        )
      }
    }
    #Add Config File
    if (exportdldsettings$IncConfig == TRUE){
      updateProgressBar(
        id = "dldprogress",
        session = session,
        value = 70,
        status = "success",
        title = "Packaging configuration file"
      )
      
      zip_append(
        zipfile = fname,
        files = "config/baseconfig.RData",
        include_directories = FALSE,
        recurse = FALSE,
        mode = "cherry-pick"
      )
    }
    
    if (exportdldsettings$IncMeta == TRUE){
      updateProgressBar(
        id = "dldprogress",
        session = session,
        value = 80,
        status = "success",
        title = "Packaging metadata"
      )
      
      
      write.csv(metadatafinal(),paste0("temp/",dldnamestart,"_metadata.csv"),row.names = FALSE)
      write.csv(qcsettingsfinal(),paste0("temp/",dldnamestart,"_qcsettings.csv"),row.names = FALSE)
      
      zip_append(
        zipfile = fname,
        files = c(paste0("temp/",dldnamestart,"_metadata.csv"),paste0("temp/",dldnamestart,"_qcsettings.csv")),
        include_directories = FALSE,
        recurse = FALSE,
        mode = "cherry-pick"
      )
    }
    
    if(exportdldsettings$IncRep == TRUE){
      updateProgressBar(
        id = "dldprogress",
        session = session,
        value = 90,
        status = "success",
        title = "Packaging report"
      )
      
      reportfile = paste0(getwd(),"/temp/",dldnamestart,"_report.html")
      
      zip_append(
        zipfile = fname,
        files = reportfile,
        include_directories = FALSE,
        recurse = FALSE,
        mode = "cherry-pick"
      )
    }
    
    updateProgressBar(
      id = "dldprogress",
      session = session,
      value = 100,
      status = "success",
      title = "Download complete"
    )
  },
  contentType = "application/zip"
)