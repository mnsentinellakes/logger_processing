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

finaldata = reactiveVal()

#Process data for export
observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    validate(
      need(selectexportdata(),"Loading...")
    )
    
    deploydatesupdate()
    #Export Data Settings
    exportsettings = selectexportdata()
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
    
    #Rename UnitID Field
    names(startfields)[1] = exportsettings$UnitID
    
    #Format Date Fields
    #Set TZ
    startfields[,2] = with_tz(
      time = startfields[2],
      tzone = exportsettings$TZ
    )
    print(2)
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
      #input$procmodel sourced from processingUI.R
      # modname = loggerfilesettings$Logger_Model[which(loggerfilesettings$ModelID == input$procmodel)]
      startfields$modname = storeloggermodel()
      names(startfields)[ncol(startfields)] = exportsettings$ModelName
    }
    
    #Add Program Name
    if (exportsettings$IncProgramName == TRUE){
      #input$procprogram sourced from processingUI.R
      # progname = programsettings$Program_Name[which(programsettings$ProgramID == input$procprogram)]
      startfields$progname = storeprogram()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramName
    }
    
    #Add WaterBody ID
    if (exportsettings$IncProgramWBID == TRUE){
      #input$procwaterbody sourced from processingUI.R
      # startfields$wbid = programwbssettings$ProgramWaterbodyID[which(programwbssettings$AppID == input$procwaterbody)]
      startfields$wbid = storewbid()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramWBID
    }
    
    #Add WaterBody Name
    if (exportsettings$IncWBName == TRUE){
      #input$procwaterbody sourced from processingUI.R
      # startfields$wbname = wbnamessettings$Waterbody_Name[which(wbnamessettings$AppID == input$procwaterbody)]
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
      #input$procstations sourced from processingUI.R
      # startfields$stationid = stationssettings$ProgramStationID[which(stationssettings$StationID == input$procstationname)]
      startfields$stationid = storestationid()
      names(startfields)[ncol(startfields)] = exportsettings$ProgramStationID
    }
    
    #Add Station Name
    if (exportsettings$IncStationName == TRUE){
      #input$procstations sourced from processingUI.R
      # startfields$stationname = stationssettings$Station_Name[which(stationssettings$StationID == input$procstationname)]
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
      #input$deploynum sourced from processingUI.R
      # startfields$deploy = unique(deploylogssettings$Deployment_Count[which(deploylogssettings$DeployID == deployid())])
      startfields$deploy = storedepcount()
      names(startfields)[ncol(startfields)] = exportsettings$Deployment
    }
    
    #Add User Name
    if (exportsettings$IncUser == TRUE){
      #input$username sourced from processingUI.R
      # startfields$user = unique(deploylogssettings$Processedby[which(deploylogssettings$DeployID == deployid())])
      startfields$user = storeuser()
      names(startfields)[ncol(startfields)] = exportsettings$User
    }
    
    
    if (exportsettings$FileSep == "Single"){
    
      combinefields = startfields
      #qcloggertypes() sourced from processing.R
      for (i in qcloggertypes()){
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
        qcdata = qcdata[,c(15:19)]
        
        if (length(qcloggertypes()) > 1){
          loggertypename = as.character(dplyr::select(exportsettings,matches(i)))
          names(qcdata) = paste0(loggertypename,"_",names(qcdata))
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
          loggertypename = as.character(dplyr::select(exportsettings,matches(i)))
          qcflags = c("FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
          qcflags = paste0(loggertypename,"_",qcflags)
          fieldorder = c(fieldorder,qcflags)
        }
      }else{
        fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
      }
      
      combinefields = combinefields[,fieldorder]
      
      finaldata(combinefields)
    }else if (exportsettings$FileSep == "Multiple"){
      combinelist = list()
      for (i in qcloggertypes()){
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
        qcdata = qcdata[,c(15:19)]
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
        #Add Parameters
        datafieldname = as.character(dplyr::select(exportsettings,matches(i)))
        fieldorder = c(fieldorder,datafieldname)
        
        #Add QC
        fieldorder = c(fieldorder,"FlagGross","FlagSpike","FlagRoC","FlagFlat","FlagVis")
        
        combinedata = combinedata[,fieldorder]
        
        combinelist[[i]] = combinedata
        
      }
      finaldata(combinelist)
    }

    #Clear VisQCdata
    
    if (exportsettings$IncSum == FALSE){
      VisQCdata(NULL)
    }
  }
)

summarydata = reactiveVal()

observeEvent(
  input$exportprocessbttn,
  ignoreNULL = FALSE,{
    validate(
      need(selectexportdata(),"Loading...")
    )
    sumexportsettings = selectexportdata()

    if (sumexportsettings$IncSum == TRUE){
          
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
 
          sumdataz.xts = as.xts(as.numeric(as.numeric(origzselect$Data)),order.by = origzselect$DateTime)
          
          sumdataz.mean.xts = apply.daily(sumdataz.xts,"mean")
          sumdataz.max.xts = apply.daily(sumdataz.xts,max)
          sumdataz.min.xts = apply.daily(sumdataz.xts,min)
          sumdataz.sd.xts = apply.daily(sumdataz.xts,sd)
          sumdataz.count.xts = apply.daily(sumdataz.xts,nrow)
          
          sumdatazmean = data.frame("unitid" = origzunitid,"datetime" = index(sumdataz.mean.xts),"z" = j,"mean" = coredata(sumdataz.mean.xts))
          sumdatazmax = data.frame("unitid" = origzunitid,"datetime" = index(sumdataz.max.xts),"z" = j,"max" = coredata(sumdataz.max.xts))
          sumdatazmin = data.frame("unitid" = origzunitid,"datetime" = index(sumdataz.min.xts),"z" = j,"min" = coredata(sumdataz.min.xts))
          sumdatazsd = data.frame("unitid" = origzunitid,"datetime" = index(sumdataz.sd.xts),"z" = j,"sd" = coredata(sumdataz.sd.xts))
          sumdatazcount = data.frame("unitid" = origzunitid,"datetime" = index(sumdataz.count.xts),"z" = j,
                                     "count" = coredata(sumdataz.count.xts))
 
          sumdatazday = left_join(sumdatazmean,sumdatazmax,by = c("unitid","datetime","z"))
          sumdatazday = left_join(sumdatazday,sumdatazmin,by = c("unitid","datetime","z"))
          sumdatazday = left_join(sumdatazday,sumdatazsd,by = c("unitid","datetime","z"))
          sumdatazday = left_join(sumdatazday,sumdatazcount,by = c("unitid","datetime","z"))

          sumdataz = rbind(sumdataz,sumdatazday)
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
        #input$procmodel sourced from processingUI.R
        # summodname = sumloggerfilesettings$Logger_Model[which(sumloggerfilesettings$ModelID == input$procmodel)]
        sumstartfields$summodname = storeloggermodel()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ModelName
      }

      #Add Program Name
      if (sumexportsettings$IncProgramName == TRUE){
        #input$procprogram sourced from processingUI.R
        # sumprogname = sumprogramsettings$Program_Name[which(sumprogramsettings$ProgramID == input$procprogram)]
        sumstartfields$progname = storeprogram()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramName
      }

      #Add WaterBody ID
      if (sumexportsettings$IncProgramWBID == TRUE){
        #input$procwaterbody sourced from processingUI.R
        # sumstartfields$wbid = sumprogramwbssettings$ProgramWaterbodyID[which(sumprogramwbssettings$AppID == input$procwaterbody)]
        sumstartfields$wbid = storewbid()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramWBID
      }

      #Add WaterBody Name
      if (sumexportsettings$IncWBName == TRUE){
        #input$procwaterbody sourced from processingUI.R
        # sumstartfields$wbname = sumwbnamessettings$Waterbody_Name[which(sumwbnamessettings$AppID == input$procwaterbody)]
        sumstartfields$wbname = storewbname()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$WBName
      }

      #Add WaterBody Type
      if (sumexportsettings$IncWBType == TRUE){
        #input$procwaterbody sourced from processingUI.R
        sumstartfields$wbtype = sumwbnamessettings$programwbssettings$WB_Type[which(sumprogramwbssettings$AppID == input$procwaterbody)]
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$WBType
      }

      #Add Station ID
      if (sumexportsettings$IncProgramStationID == TRUE){
        #input$procstations sourced from processingUI.R
        # sumstartfields$stationid = sumstationssettings$ProgramStationID[which(sumstationssettings$StationID == input$procstationname)]
        sumstartfields$stationid = storestationid()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$ProgramStationID
      }

      #Add Station Name
      if (sumexportsettings$IncStationName == TRUE){
        #input$procstations sourced from processingUI.R
        # sumstartfields$stationname = sumstationssettings$Station_Name[which(sumstationssettings$StationID == input$procstationname)]
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
        #input$deploynum sourced from processingUI.R
        # sumstartfields$deploy = unique(sumdeploylogssettings$Deployment_Count[which(sumdeploylogssettings$DeployID == deployid())])
        sumstartfields$deploy = storedepcount()
        names(sumstartfields)[ncol(sumstartfields)] = sumexportsettings$Deployment
      }

      #Add User Name
      if (sumexportsettings$IncUser == TRUE){
        #input$username sourced from processingUI.R
        # sumstartfields$user = unique(sumdeploylogssettings$Processedby[which(sumdeploylogssettings$DeployID == deployid())])
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
        if (summexportsettings$IncLoc == TRUE){
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
      VisQCdata(NULL)
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
    #Delete Temp folder
    unlink(paste0(getwd(),"/temp"),recursive = FALSE)
    
    #Create Temp folder
    dir.create("temp")
    
    #Get Waterbody Name
    wbnamedld = wbnames()
    dldname = storewbname()
    
    #Process Data for Single File
    if (exportdldsettings$FileSep == "Single"){
      
      dldnamestart = paste0(dldname)
      
      dldnamestart = gsub("\\s+","_",gsub("[[:punct:]]","",dldnamestart))
      
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
      
      #List containing file paths
      listoutput = NULL
      for (i in listnames){
        selectfinaldata = finaldataout[[i]]
        dldname = paste0(dldname,collapse = "_")
        dldname = gsub("\\s+","_",gsub("[[:punct:]]","",dldname))
        dldname1 = paste0(dldname,"_",i,".csv")
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
        summarydataout = summarydata()
        sumlistnames = names(summarydataout)
        
        sumlistoutput = NULL
        for (i in sumlistnames){
          selectsumdata = summarydataout[[i]]
          sumdldname = paste0(dldname,coll = "_")
          sumdldname = gsub("\\s+","_",gsub("[[:punct:]]","",sumdldname))
          sumdldnameend = paste0(sumdldname,"_",i,"_summary.csv")
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
      zip_append(
        zipfile = fname,
        files = "config/baseconfig.RData",
        include_directories = FALSE,
        recurse = FALSE,
        mode = "cherry-pick"
      )
    }
    
    if (exportdldsettings$IncMeta == TRUE){
      buildmeta(
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
      
      getqcsettings(
        appid = input$procwaterbody,
        stationid = input$procstationname,
        deployid = deployid(),
        deploylogs = deploylogs(),
        qcconfig = qc_config(),
        programwbs = programwbs(),
        wbnames = wbnames(),
        stations = stations()
      )
      
      zip_append(
        zipfile = fname,
        files = c("temp/metadata.csv","temp/qcsettings.csv"),
        include_directories = FALSE,
        recurse = FALSE,
        mode = "cherry-pick"
      )
    }
  },
  contentType = "application/zip"
)