#Collect and build metadata for export
buildmeta = function(programid,appid,stationid,deployid,modelid,programs,programwbs,wbnames,stations,logfiledefs,deploylogs,qcconfig,export){
  
  #Program Name
  progname = programs$Program_Name[which(programs$ProgramID == programid)]
  
  #WaterbodyID
  wbid = programwbs$ProgramWaterbodyID[which(programwbs$AppID == appid)]
  
  #Waterbody Name
  wbname = wbnames$Waterbody_Name[which(wbnames$AppID == appid)]
  
  #Waterbody Type
  wbtype = programwbs$WB_Type[which(programwbs$AppID == appid)]
  
  #StationID
  if (is.na(stations$ProgramStationID[which(stations$StationID == stationid)])){
    statid = NA
  }else{
  statid = stations$ProgramStationID[which(stations$StationID == stationid)]
  }
  
  #Station Name
  if (is.na(stations$Station_Name[which(stations$StationID == stationid)])){
    statname = NA
  }else{
    statname = stations$Station_Name[which(stations$StationID == stationid)]
  }
  
  #Model Name
  modname = logfiledefs$Logger_Model[which(logfiledefs$ModelID == modelid)]
  
  #Logger Types
  logtypes = paste(unique(deploylogs$Logger_Type[which(deploylogs$DeployID == deployid)]),collapse = ",")
  
  #Units
  units = paste(unique(deploylogs$Units[which(deploylogs$DeployID == deployid)]),collapse = ",")
  
  #Recorded Start
  recstart = unique(deploylogs$StartDateTimeRecord[which(deploylogs$DeployID == deployid)])
  
  #Recorded End
  recend = unique(deploylogs$EndDateTimeRecord[which(deploylogs$DeployID == deployid)])
  
  #Valid Start
  valstart = unique(deploylogs$StartDateTimeValid[which(deploylogs$DeployID == deployid)])
  
  #Valid End
  valend = unique(deploylogs$EndDateTimeValid[which(deploylogs$DeployID == deployid)])
  
  #Time Zone
  tz = export$TZ[which(export$ProgramID == programid & export$ModelID == modelid)]
  
  #Logger Count
  logcount = unique(deploylogs$Logger_Count[which(deploylogs$DeployID == deployid)])
  
  #Z values
  if (is.na(unique(deploylogs$Z[which(deploylogs$DeployID == deployid)])) | 
      unique(deploylogs$Z[which(deploylogs$DeployID == deployid)]) == ""){
    zs = NA
  }else{
    zs = unique(deploylogs$Z[which(deploylogs$DeployID == deployid)])
  }
  
  #Deployment Count
  deploycount = unique(deploylogs$Logger_Count[which(deploylogs$DeployID == deployid)])
  
  #Process Date
  procdate = unique(deploylogs$ProcessedDate[which(deploylogs$DeployID == deployid)])
  
  #User
  if (unique(deploylogs$Processedby[which(deploylogs$DeployID == deployid)]) == ""){
    user = NA
  }else{
    user = unique(deploylogs$Processedby[which(deploylogs$DeployID == deployid)])
  }
  
  
  metadataframe = data.frame(
    "Program" = progname,
    "WaterbodyID" = wbid,
    "Waterbody_Name" = wbname,
    "Waterbody_Type" = wbtype,
    "StationID" = statid,
    "Station_Name" = statname,
    "Model_Name" = modname,
    "Logger_Types" = logtypes,
    "Units" = units,
    "Recording_Start" = recstart,
    "Recording_End" = recend,
    "Valid_Start" = valstart,
    "Valid_End" = valend,
    "Time_Zone" = tz,
    "Logger_Count" = logcount,
    "Z_Values" = zs,
    "Deployment_Number" = deploycount,
    "Date_Processed" = procdate,
    "Processed_By" = user
  )
  
  write.csv(metadataframe,"temp/metadata.csv",row.names = FALSE)
}

#Compile QC Settings for export
getqcsettings = function(appid,stationid,deployid,deploylogs,qcconfig,programwbs,wbnames,stations){
  
  selectdeploy = deploylogs
  #deployid() sourced from processing.R
  #Select Logger Type
  selectdeploylogger = unique(selectdeploy$Logger_Type[which(selectdeploy$DeployID == deployid)])
  #Select Deployment Count
  selectdeploycount = unique(selectdeploy$Deployment_Count[which(selectdeploy$DployID == deployid)])
  
  selectstation = stations
  selectstation = stations[which(stations$StationID == stationid),]
  selectstationprogid = selectstation$ProgramStationID
  selectstationname = selectstation$Station_Name
  
  #Select QC Data
  selectqcdata = qcconfig
  #input$procwaterbody sourced from processingUI.R
  selectqcdata = selectqcdata[which(selectqcdata$AppID == input$procwaterbody & selectqcdata$Logger_Type %in% selectdeploylogger),]
  qcwbid = programwbs$ProgramWaterbodyID[which(programwbs$AppID == appid)]
  qcwbname = wbnames$Waterbody_Name[which(wbnames$AppID == appid)]
  selectqcdata$AppID = NULL
  #Add Fields
  if (!is.na(qcwbid)){
    selectqcdata$WaterbodyID = qcwbid
  }
  if (!is.na(qcwbname)){
    selectqcdata$WaterbodyName = qcwbname
  }
  if(!is.na(selectstationprogid)){
    selectqcdata$StationID = selectstationprogid
  }
  if(!is.na(selectstationname)){
    selectqcdata$StationName = selectstationname
  }
  
  # selectqcdata = selectqcdata[,c(5,6,1:4)]
  
  write.csv(selectqcdata,"temp/qcsettings.csv",row.names = FALSE)
}
