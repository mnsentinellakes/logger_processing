turnoffprocessupdate = TRUE


procwbs = reactive({
  wbs = programwbs()
  wbname = wbnames()
  wbs = wbs[which(wbs$AppID == input$procwaterbody),]
  wbname = wbname$Waterbody_Name[which(wbname$AppID == input$procwaterbody)]
  wbs = cbind(wbs,wbname)
  
  return(wbs)
})

#Format Raw input data for QC
#datafilepath - raw input data file
#siteid - Logger Serial Number
#waterbody - selected waterbody
#model - selected model
#loggerdefs - logger file definition table

formatforQC = function(datafilepath,siteid,waterbody,loggermodel,loggerfields,qcunits){
  
  #Function for reading data
  readloggerdata = function(loggerfile,loggerfields){
    
    skiprows = loggerfields$FieldNamesRow-1
    if (skiprows==0){
      skiprows=FALSE
    }
    
    datafile = read.table(
      header = TRUE,
      file = loggerfile,
      sep = ",",
      skip = skiprows,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
    
    if (loggerfields$DataStartRow-loggerfields$FieldNamesRow != 1){
      endrow = loggerfields$DataStartRow-2
      delrow = seq(1,endrow)
      datafile = datafile[-delrow,]
    }
    
    return(datafile)
  }
  
  #Filter qc_config field by waterbody
  qcunits = qcunits[which(qcunits$AppID == waterbody),]
  
  #Select which fields should be included
  loggerfields = loggerfields[which(loggerfields$ModelID == loggermodel),]
  timezone = loggerfields$TZ
  
  #Select datetime fields
  datetimefields = Filter(function(x)!all(is.na(x)),loggerfields[,2:4])
  datetimefieldnames = data.frame("qcfield"=names(datetimefields),"datafield"=unname(unlist(datetimefields[1,])),stringsAsFactors = FALSE)
  
  #Select data fields
  datafields = Filter(function(x)!all(is.na(x)),loggerfields[,c(8:18)])
  
  
  datafieldnames = data.frame("qcfield"=names(datafields),"datafield"=unname(unlist(datafields[1,])),stringsAsFactors = FALSE)
  datafieldnames = datafieldnames[which(nchar(datafieldnames$datafield)>0),]
  
  qctypes = datafieldnames$qcfield
  
  #Add units to to datafields table
  datafieldnames$Units = NA
  for (i in 1:nrow(datafieldnames)){
    datafieldnames[i,3] = qcunits$Units[which(qcunits$Logger_Type == datafieldnames[i,1] & qcunits$QC_Metric == "Gross.Fail.Hi")]
    print(1)
  }
  datafieldnames$type = datafieldnames$qcfield
  datafieldnames$qcfield = paste0(datafieldnames$qcfield,datafieldnames$Units)
  datafieldnames$Units=NULL
  
  #Read Data
  datafile = readloggerdata(
    loggerfile = datafilepath,
    loggerfields = loggerfields
  )
  
  #Throw error if DateTime doesnt match
  
  #Standardize field names
  datetimefieldnames$qcfield = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(datetimefieldnames$qcfield),perl = TRUE))
  datetimefieldnames$datafield = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(datetimefieldnames$datafield),perl = TRUE))
  datafieldnames$qcfield = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(datafieldnames$qcfield),perl = TRUE))
  datafieldnames$datafield = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(datafieldnames$datafield),perl = TRUE))
  print(2)
  names(datafile) = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(names(datafile)),perl = TRUE))
  print(names(datafile))
  #Rebuild dataset
  
  for (i in 1:nrow(datafieldnames)){
    
    builddata=data.frame("RowID" = seq(1:nrow(datafile)))
    
    builddatacolumn = data.frame(datafile[which(names(datafile) == datafieldnames$datafield[i])])
    
    
    names(builddatacolumn) = datafieldnames$qcfield[i]
    builddata = cbind(builddata,builddatacolumn)
    
    
    #Convert Date and Time to correct format
    datetimeformat = paste0(loggerfields$Date_Format," ",loggerfields$Time_Format)
    if ("DateTime" %in% datetimefieldnames$qcfield){
      
      builddatecolumn = datafile[which(names(datafile) == datetimefieldnames$datafield[which(datetimefieldnames$qcfield == "DateTime")])]
      
    }else{
      builddatecolumn = paste0(datafile[which(names(datafile) == datetimefieldnames[which(datetimefieldnames$qcfield) == "Date"])]," ",
                               datafile[which(names(datafile) == datetimefieldnames[which(datetimefieldnames$qcfield) == "Time"])])
    }
    
    names(builddatecolumn) = "DateTime"
    builddata = cbind(builddata,builddatecolumn)
    
    builddata$DateTime = as.POSIXct(builddata$DateTime,format = datetimeformat,tz = timezone)
    
    #Add SiteID
    builddata$SiteID = siteid
    
    #Extract start and end dates to name the file
    startdate = gsub("-","",as.character(min(as.Date(builddata$DateTime))))
    enddate = gsub("-","",as.character(max(as.Date(builddata$DateTime))))
    
    foldername = paste0("processing/",datafieldnames$type[i])
    
    dir.create(foldername,showWarnings = FALSE)
    write.csv(builddata,paste0(foldername,"/",siteid,"_Water_",startdate,"_",enddate,".csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")
  }
  
  #Compile QC metadata
  qcinfo = list("qcnames" = qctypes,"startdate" = as.character(max(as.Date(builddata$DateTime))),
                "enddate" = as.character(min(as.Date(builddata$DateTime))))
  
}

#Run QC Processes on data
#Siteid == Serial_Number
QCProcess = function(qcinfo,siteid){
  
  loggertypes = qcinfo$qcnames
  
  if (!is.na(qcinfo$startdate) & !is.na(qcinfo$enddate)){
  for (i in loggertypes){
    dir.create(paste0("processing/",i,"/QC"),showWarnings = FALSE)
    
    print(qcinfo$startdate)
    
    ContDataQC::ContDataQC(
      fun.myData.Operation = "QCRaw",
      fun.myDir.import = paste0("processing/",i),
      fun.myDir.export = paste0("processing/",i,"/QC"),
      fun.myData.Type = "Water",
      fun.myData.DateRange.Start = qcinfo$startdate,
      fun.myData.DateRange.End = qcinfo$enddate,
      fun.myData.SiteID = siteid,
      fun.CreateReport = FALSE,
      fun.myConfig = "config/configfile.R"
    )
  }
  }else{
    #Incorrect Date Format Error
    sendSweetAlert(
      session = session,
      title = "Incorrect Date Format",
      text = "Please ensure the date format in the Logger File Definitions match the input logger data",
      type = "error"
    )
    
  }
}

#import QC data and apply depths
compileQCdata = function(qcinfo,depthstable){
  loggertypes = qcinfo$qcnames
  
  siteids = depthstable$UnitID
  
  stopprocess = FALSE
  datalist = list()
  for (i in loggertypes){
    loggertypecompile = NULL
    for (j in siteids){
      
      if (stopprocess == FALSE){
      qcpath = paste0("processing/",i,"/QC/")
      qcfiles = list.files(qcpath,full.names = TRUE)
      qcfile = qcfiles[which(grepl(j,qcfiles))]
      
      if(length(qcfile)>0){
      readdata = read.csv(qcfile,stringsAsFactors = FALSE)
      
      datacompile = data.frame("UnitID" = j,
                               "DateTime" = as.POSIXct(readdata$DateTime,format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                               "Data" = readdata[,ncol(readdata)],
                               "Z" = depthstable$Z[which(depthstable$UnitID == j)],
                               "FlagGrossorig" = readdata[,which(grepl("Flag.Gross",names(readdata)))],
                               "FlagSpikeorig" = readdata[,which(grepl("Flag.Spike",names(readdata)))],
                               "FlagRoCorig" = readdata[,which(grepl("Flag.RoC",names(readdata)))],
                               "FlagFlatorig" = readdata[,which(grepl("Flag.Flat",names(readdata)))],
                               "FlagVisorig" = "P",
                               "FlagGrosschng" = as.character(NA),
                               "FlagSpikechng" = as.character(NA),
                               "FlagRoCchng" = as.character(NA),
                               "FlagFlatchng" = as.character(NA),
                               "FlagVischng" = as.character(NA),
                               "FlagGross" = readdata[,which(grepl("Flag.Gross",names(readdata)))],
                               "FlagSpike" = readdata[,which(grepl("Flag.Spike",names(readdata)))],
                               "FlagRoC" = readdata[,which(grepl("Flag.RoC",names(readdata)))],
                               "FlagFlat" = readdata[,which(grepl("Flag.Flat",names(readdata)))],
                               "FlagVis" = "P",
                               stringsAsFactors = FALSE)
      
      loggertypecompile = rbind(loggertypecompile,datacompile)
      }else{
        
        sendSweetAlert(
          session = session,
          title = paste("No file with ID:",j),
          text = "Ensure the IDs in the Depths Table match the file names",
          type = "error"
        )
        stopprocess = TRUE
    }
    datalist[[i]] = loggertypecompile
      }else{}
    }
  }
  
  finaloutput = list(datalist,stopprocess)
  
  # save(datalist,file = "C:/Projects/Shiny_App_Development/Logger_Processing/Test/datalisttest.RDATA")
  return(finaloutput)
}

#Create Reactive Value for VisQCdata step
VisQCdata=reactiveVal()

#Create Reactive to store current deployid
deployid = reactiveVal()

#Process and QC data----
#Observes the Processing button and begins reformating and QCing the data

#Run Data Processing and QC
observeEvent(
  input$processing,{
    
    #Disable processing button to prevent duplicate processing
    toggleState("processing")
    unlink("processing/*",recursive = TRUE,force = TRUE)
    #Ensures that data have been uploaded
    if(length(input$dataupload)>0){
      
      ##Data QC and Processing
      #Table with information about the uploaded data
      filetable=input$dataupload
      
      #Temp paths to the uploaded data
      datapaths=filetable$datapath
      #Logger lookup table for associating loggers to depths
      depthstable=processinglogs()
      
      #Selects lookup table rows for selected lake and that have not been processed
      depthstableselect=depthstable[which(depthstable$StationID == input$procstationname & depthstable$ModelID == input$procmodel & 
                                            is.na(depthstable$Processed)),]
      
      #Progress Update 1
      updateProgressBar(
        id = "processprogress",
        session = session,
        value = 5,
        status = "success",
        title = "Start Processing"
      )
      
      
      progvaltotal = 75 / (2 * length(datapaths))
      progval = 5
      #Iterates through the uploaded files, reformats them and runs QC
      for (j in 1:length(datapaths)){
        
        inputname=as.character(filetable$name[j])
        inputname=as.character(gsub(".csv","",inputname))
        inputname=as.character(gsub(".TXT","",inputname))
        inputname=as.character(gsub(".txt","",inputname))
        
        if (inputname %in% depthstableselect$UnitID){
          
          file.create("processing",showWarnings = FALSE)
          
          progval = progval + progvaltotal
          
          updateProgressBar(
            id = "processprogress",
            session = session,
            value = progval,
            status = "success",
            title = paste("Formatting",inputname,"for QC")
          )
          
          
          qcinfo = formatforQC(
            datafilepath = datapaths[j],
            siteid = inputname,
            waterbody = input$procwaterbody,
            loggermodel = input$procmodel,
            loggerfields = loggerfiledefs(),
            qcunits = qc_config()
          )
          
          progval = progval + progvaltotal
          
          updateProgressBar(
            id = "processprogress",
            session = session,
            value = progval,
            status = "success",
            title = paste("Running automatic QC checks for",inputname)
          )
          
          
          
          QCProcess(
            qcinfo = qcinfo,
            siteid = inputname
          )
          stopqc = FALSE
        }else{
          sendSweetAlert(
            session = session,
            title = "Missing Logger UnitID",
            text = paste("Logger",inputname,"has not been included in the Logger Units Table. Ensure that the correct Logger Model has been
                       selected or enter the UnitID in to the Logger Units Table"),
            type = "error"
          )
          stopqc = TRUE
        }
      }
      
      if (stopqc == FALSE){
      updateProgressBar(
        id = "processprogress",
        session = session,
        value = 85,
        status = "success",
        title = paste("Compiling and formatting QCed data for",inputname)
      )
      
      compiledata = compileQCdata(
        qcinfo = qcinfo,
        depthstable = depthstableselect
      )
      
      if(compiledata[[2]] == FALSE){
        
        compiledata = compiledata[[1]]
        
        VisQCdata(compiledata)
        
        updateProgressBar(
          id = "processprogress",
          session = session,
          value = 90,
          status = "success",
          title = paste("Saving Metadata")
        )
        
        ##Update tables
        #Load tables
        updatedates = processinglogs()
        deployadd = deploylogs()
        qctable = qc_config()
        stationupdate = stations()
        
        #Update the depths table processed date
        
        if (turnoffprocessupdate == FALSE){
          updatedates$Processed[which(updatedates$StationID == input$procstationname &
                                        updatedates$ModelID == input$procmodel & is.na(updatedates$Processed))] = Sys.Date()
          processinglogs(updatedates)
        }
        #Update deploy table
        deployidcreate = random_id(n = 1,bytes = 16)
        deployid(deployidcreate)
        
        deployaddrows = NULL
        for (k in names(compiledata)){
          selectunit = unique(qctable$Units[which(qctable$AppID == input$procwaterbody & qctable$Logger_Type == k)])
          selectunit = selectunit[!is.na(selectunit)]
          
          
          deployaddrow = data.frame(
            "DeployID" = deployidcreate,
            "Logger_Type" = k,
            "Lat" = input$lat,
            "Lon" = input$lon,
            "StartDateTimeRecord" = as.character(NA),
            "EndDateTimeRecord" = as.character(NA),
            "StartDateTimeValid" = as.character(NA),
            "EndDateTimeValid" = as.character(NA),
            "Units" = selectunit,
            "Logger_Count" = length(datapaths),
            "Deployment_Count" = input$deploynum,
            "ProcessedDate" = as.character(Sys.Date()),
            "Processedby" = input$username,
            stringsAsFactors = FALSE
          )

          deployaddrows = rbind(deployaddrows,deployaddrow)
        }
        deployadd = rbind(deployadd,deployaddrows)

        deploylogs(deployadd)
        
        #Update stations table
        stationupdate$Lat[which(stationupdate$StationID == input$procstationname)] = input$lat
        stationupdate$Lon[which(stationupdate$StationID == input$procstationname)] = input$lon
        
        stations(stationupdate)
        
        updatebaseconfig()
        
        updateProgressBar(
          id = "processprogress",
          session = session,
          value = 100,
          status = "success",
          title = paste("Processing and QC complete")
        )
      }else{
        updateProgressBar(
          id = "processprogress",
          session = session,
          value = 90,
          status = "danger",
          title = paste("Delete and Re-enter Unit IDs")
        )
      }
    }
    }else{
      sendSweetAlert(
        session,
        title = "Missing Logger Data",
        text = "Please upload logger data",
        type = "error"
      )
    }
    
    unlink("processing/*",recursive = TRUE,force = TRUE)
    
  })

#Re-enable disabled Processing button upon uploading new data
observeEvent(
  input$dataupload,{
    toggleState("processing")
  }
)

#Extract logger types
qcloggertypes = reactive({
  qcloggers=VisQCdata()
  qcloggers = names(qcloggers)
  
  return(qcloggers)
})

#Render Data Table for Review of Processed Data
output$dataoutput=renderDT(
  options=list(
    lengthChange = FALSE
  ),
  extensions = 'Responsive',
  {

    qcdatadisplay=VisQCdata()
    datatypeselect = head(qcdatadisplay[[input$prevloggerchoices]])
    
    return(datatypeselect[,c(1:4,15:18)])
  })