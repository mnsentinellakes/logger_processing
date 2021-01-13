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
      modname = loggerfilesettings$Logger_Model[which(loggerfilesettings$ModelID == input$procmodel)]
      startfields$modname = modname
      names(startfields)[ncol(startfields)] = exportsettings$ModelName
    }
    
    #Add Program Name
    if (exportsettings$IncProgramName == TRUE){
      #input$procprogram sourced from processingUI.R
      progname = programsettings$Program_Name[which(programsettings$ProgramID == input$procprogram)]
      startfields$progname = progname
      names(startfields)[ncol(startfields)] = exportsettings$ProgramName
    }
    
    #Add WaterBody ID
    if (exportsettings$IncProgramWBID == TRUE){
      #input$procwaterbody sourced from processingUI.R
      startfields$wbid = programwbssettings$ProgramWaterbodyID[which(programwbssettings$AppID == input$procwaterbody)]
      names(startfields)[ncol(startfields)] = exportsettings$ProgramWBID
    }
    
    #Add WaterBody Name
    if (exportsettings$IncWBName == TRUE){
      #input$procwaterbody sourced from processingUI.R
      startfields$wbname = wbnamessettings$Waterbody_Name[which(wbnamessettings$AppID == input$procwaterbody)]
      names(startfields)[ncol(startfields)] = exportsettings$WBName
    }
    
    if (exportsettings$IncWBType == TRUE){
      #input$procwaterbody sourced from processingUI.R
      startfields$wbtype = wbnamessettings$programwbssettings$WB_Type[which(programwbssettings$AppID == input$procwaterbody)]
      names(startfields)[ncol(startfields)] = exportsettings$WBType
    }
    
    #Add Station ID
    if (exportsettings$IncProgramStationID == TRUE){
      #input$procstations sourced from processingUI.R
      startfields$stationid = stationssettings$ProgramStationID[which(stationssettings$StationID == input$procstation)]
      names(startfields)[ncol(startfields)] = exportsettings$ProgramStationID
    }
    
    #Add Station Name
    if (exportsettings$IncStationName == TRUE){
      #input$procstations sourced from processingUI.R
      startfields$stationname = stationssettings$Station_Name[which(stationssettings$StationID == input$procstationname)]
      names(startfields)[ncol(startfields)] = exportsettings$StationName
    }
    
    #Add Deployment Count
    if (exportsettings$IncDeploy == TRUE){
      
      #input$deploynum sourced from processingUI.R
      startfields$deploy = input$deploynum
      names(startfields)[ncol(startfields)] = exportsettings$Deployment
    }
    
    #Add User Name
    if (exportsettings$IncUser == TRUE){
      #input$username sourced from processingUI.R
      startfields$user = input$username
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
          names(qcdata) = paste0(loggertypename,names(qcdata))
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
          qcflags = paste0(loggertypename,qcflags)
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
  }
)

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
    dir.create(paste0(getwd(),"/temp"))
    
    #Get Waterbody Name
    wbnamedld = wbnames()
    dldname = wbnamedld$Waterbody_Name[which(wbnamedld$AppID == input$procwaterbody)]
    
    #Process Data for Single File
    if (exportdldsettings$FileSep == "Single"){
      dldname = paste0(dldname,"_",qcloggertypes(),collapse = "_")
      dldname = gsub("\\s+","_",gsub("[[:punct:]]","",dldname))
      dldname = paste0(dldname,".csv")
      dldpath = paste0("temp/",dldname)
      
      #Create the csv and zip it
      write.csv(finaldata(),dldpath,row.names = FALSE)
      zip(
        zipfile = fname,
        files = dldpath,
        include_directories = FALSE,  
        recurse = FALSE,
        mode = "cherry-pick"
      )
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
        deployid = deployid(),
        deploylogs = deploylogs(),
        qcconfig = qc_config(),
        programwbs = programwbs(),
        wbnames = wbnames()
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