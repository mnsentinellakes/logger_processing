turnoffprocessupdate = FALSE

#Format Raw input data for QC
#datafilepath - raw input data file
#siteid - Logger Serial Number
#waterbody - selected waterbody
#model - selected model
#loggerdefs - logger file definition table

formatforQC = function(datafilepath,siteid,waterbody,loggermodel,loggerfields,qcunits){
  
  #Function for reading data
  readloggerdata = function(loggerfile,loggerfields){
    skiprows = loggerfields$FieldNamesRow - 1
    if (skiprows == 0){
      skiprows = FALSE
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
      endrow = loggerfields$DataStartRow - 2
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
  datafieldnames = datafieldnames[which(nchar(datafieldnames$datafield) > 0),]
  qctypes = datafieldnames$qcfield
  
  #Add units to to datafields table
  datafieldnames$Units = NA
  for (i in 1:nrow(datafieldnames)){
    datafieldnames[i,3] = qcunits$Units[which(qcunits$Logger_Type == datafieldnames[i,1] & qcunits$QC_Metric == "Gross.Fail.Hi")]
  }
  datafieldnames$type = datafieldnames$qcfield
  datafieldnames$qcfield = paste0(datafieldnames$qcfield,datafieldnames$Units)
  datafieldnames$Units = NULL
  
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
  names(datafile) = gsub("Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(names(datafile)),perl = TRUE))
  
  
  
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
    
    #Determine if file should be named with Air or Water
    if (datafieldnames$type[i] %in% c("AirTemp","AirBP")){
      qctypename = "Air"
    }else{
      qctypename = "Water"
    }
    
    dir.create(foldername,showWarnings = FALSE)
    write.csv(builddata,paste0(foldername,"/",siteid,"_",qctypename,"_",startdate,"_",enddate,".csv"),row.names = FALSE,
              fileEncoding = "ISO-8859-1")
  }
  
  #Compile QC metadata
  qcinfo = list("qcnames" = qctypes,"startdate" = as.character(max(as.Date(builddata$DateTime))),
                "enddate" = as.character(min(as.Date(builddata$DateTime))),"fieldnames" = datafieldnames)
}

#Run QC Processes on data
#Siteid == Serial_Number
QCProcess = function(qcinfo,siteid){
  
  loggertypes = qcinfo$qcnames
  
  if (!is.na(qcinfo$startdate) & !is.na(qcinfo$enddate)){
    for (i in loggertypes){
      dir.create(paste0("processing/",i,"/QC"),showWarnings = FALSE)
      
      #Determine if ContDataQC should look for Air or Water in file name
      if (i %in% c("AirTemp","AirBP")){
        contdataqctype = "Air"
      }else{
        contdataqctype = "Water"
      }
      
      ContDataQC::ContDataQC(
        fun.myData.Operation = "QCRaw",
        fun.myDir.import = paste0("processing/",i),
        fun.myDir.export = paste0("processing/",i,"/QC"),
        fun.myData.Type = contdataqctype,
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
  
  datafields = qcinfo$fieldnames
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
        
        if(length(qcfile) > 0){
          readdata = read.csv(qcfile,stringsAsFactors = FALSE)
          
          datafieldname = datafields$qcfield[which(datafields$type == i)]

          datafield = readdata[[datafieldname]]
          
          datacompile = data.frame("UnitID" = j,"DateTime" = as.POSIXct(readdata$DateTime,format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),
                                   "Data" = datafield,"Z" = depthstable$Z[which(depthstable$UnitID == j)],
                                   "FlagGrossorig" = as.character(readdata[,which(grepl("Flag.Gross",names(readdata)))]),
                                   "FlagSpikeorig" = as.character(readdata[,which(grepl("Flag.Spike",names(readdata)))]),
                                   "FlagRoCorig" = as.character(readdata[,which(grepl("Flag.RoC",names(readdata)))]),
                                   "FlagFlatorig" = as.character(readdata[,which(grepl("Flag.Flat",names(readdata)))]),
                                   "FlagVisorig" = "P","FlagGrosschng" = as.character(NA),"FlagSpikechng" = as.character(NA),
                                   "FlagRoCchng" = as.character(NA),"FlagFlatchng" = as.character(NA),"FlagVischng" = as.character(NA),
                                   "FlagGross" = as.character(readdata[,which(grepl("Flag.Gross",names(readdata)))]),
                                   "FlagSpike" = as.character(readdata[,which(grepl("Flag.Spike",names(readdata)))]),
                                   "FlagRoC" = as.character(readdata[,which(grepl("Flag.RoC",names(readdata)))]),
                                   "FlagFlat" = as.character(readdata[,which(grepl("Flag.Flat",names(readdata)))]),"FlagVis" = "P",
                                   stringsAsFactors = FALSE)
          
          if(all(datacompile$FlagGrossorig == "FALSE")){
            datacompile$FlagGrossorig = "F"
            datacompile$FlagGrosschng = "F"
            datacompile$FlagGross = "F"
          }
          
          loggertypecompile = rbind(loggertypecompile,datacompile)
        }else{
          sendSweetAlert(
            session = session,
            title = paste("No file with ID:",j,"or incorrect date/time format"),
            text = "Ensure the IDs in the Depths Table match the file names and that the date/time format set in the Logger File Definitions
            tab matches the date/time format in the logger file.",
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

#Create Reactive to store processed Program
storeprogram = reactiveVal()

#Create Reactive to store processed waterbody name
storewbname = reactiveVal()

#Create Reactive to store processed waterbody id
storewbid = reactiveVal()

#Create Reactive to store processed Station Name
storestationname = reactiveVal()

#Create Reactive to store processed Station ID
storestationid = reactiveVal()

#Create Reactive to store processed Logger Model
storeloggermodel = reactiveVal()

#Create Reactive to store processed Latitude
storelat = reactiveVal()

#Create Reactive to store processed Longitude
storelon = reactiveVal()

#Create Reactive to store processed User Name
storeuser = reactiveVal()

#Create Reactive to store current deployment number
storedepcount = reactiveVal()

#Create Reactive to store current deployid
deployid = reactiveVal()



#Process and QC data----
#Observes the Processing button and begins reformating and QCing the data

#Run Data Processing and QC
observeEvent(
  input$processingbttn,{
    
    #Disable processing button to prevent duplicate processing
    toggleState("processing")
    unlink("processing/*",recursive = TRUE,force = TRUE)
    #Ensures that data have been uploaded
    if(length(input$dataupload) > 0){
      
      #Update processing reactiveVals
      #Program
      storeprogram(
        programs()$Program_Name[which(programs()$ProgramID == input$procprogram)]
      )
      print(storeprogram())
      #Waterbody Name
      storewbname(
        wbnames()$Waterbody_Name[which(wbnames()$AppID == input$procwaterbody)]
      )
      print(storewbname())
      #Waterbody ID
      storewbid(
        programwbs()$ProgramWaterbodyID[which(programwbs()$AppID == input$procwaterbody)]
      )
      print(storewbid())
      #Station Name
      storestationname(
        stations()$Station_Name[which(stations()$StationID == input$procstationname)]
      )
      print(storestationname())
      #Station ID
      storestationid(
        stations()$ProgramStationID[which(stations()$StationID == input$procstationname)]
      )
      print(storestationid())
      #Logger Model
      storeloggermodel(
        loggerfiledefs()$Logger_Model[which(loggerfiledefs()$ModelID == input$procmodel)]
      )
      print(storeloggermodel())
      #Latitude
      storelat(
        input$lat
      )
      print(storelat())
      #Longitude
      storelon(
        input$lon
      )
      print(storelon())
      #User
      storeuser(
        input$username
      )
      print(storeuser())
      #Deploy Count
      storedepcount(
        input$deploynum
      )
      print(storedepcount())
      
      ##Data QC and Processing
      #Table with information about the uploaded data
      filetable = input$dataupload
      
      #Temp paths to the uploaded data
      datapaths = filetable$datapath
      #Logger lookup table for associating loggers to depths
      depthstable = processinglogs()
      
      #Selects lookup table rows for selected lake and that have not been processed
      depthstableselect = depthstable[which(depthstable$StationID == input$procstationname & depthstable$ModelID == input$procmodel & 
                                            is.na(depthstable$Processed)),]
      
      #Progress Update 1
      updateProgressBar(
        id = "processprogress",
        session = session,
        value = 5,
        status = "success",
        title = "Start Processing"
      )
      
      #Progress bar calculation for updating while looping through input csvs
      progvaltotal = 75 / (2 * length(datapaths))
      progval = 5
      #Iterates through the uploaded files, reformats them and runs QC
      for (j in 1:length(datapaths)){
        #Format input csv name
        inputname = as.character(filetable$name[j])
        inputname = as.character(gsub(".csv","",inputname))
        inputname = as.character(gsub(".TXT","",inputname))
        inputname = as.character(gsub(".txt","",inputname))
      
        #Continue if input name is in the depthstable
        if (inputname %in% depthstableselect$UnitID){
          #Create processing folder
          dir.create("processing",showWarnings = FALSE)
          
          #Update Progress Bar
          progval = progval + progvaltotal
          updateProgressBar(
            id = "processprogress",
            session = session,
            value = progval,
            status = "success",
            title = paste("Formatting",inputname,"for QC")
          )
          
          #Run formatforQC
          qcinfo = formatforQC(
            datafilepath = datapaths[j],
            siteid = inputname,
            waterbody = input$procwaterbody,
            loggermodel = input$procmodel,
            loggerfields = loggerfiledefs(),
            qcunits = qc_config()
          )
          
          #Update Progress Bar
          progval = progval + progvaltotal
          updateProgressBar(
            id = "processprogress",
            session = session,
            value = progval,
            status = "success",
            title = paste("Running automatic QC checks for",inputname)
          )
          
          #Run QCProcess
          QCProcess(
            qcinfo = qcinfo,
            siteid = inputname
          )
          stopqc = FALSE
          
          #If the input csv name is not in the depthstable, send an alert and stop the process
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
      
      #If stopqc is not triggered, continue with process
      if (stopqc == FALSE){
        updateProgressBar(
          id = "processprogress",
          session = session,
          value = 85,
          status = "success",
          title = paste("Compiling and formatting QCed data for",inputname)
        )
        print(1)
        #Run compileQCdata
        compiledata = compileQCdata(
          qcinfo = qcinfo,
          depthstable = depthstableselect
        )
        
        if(compiledata[[2]] == FALSE){
          
          compiledata = compiledata[[1]]
          
          VisQCdata(compiledata)
          
          depthsdata = compiledata[[1]]
          
          updateProgressBar(
            id = "processprogress",
            session = session,
            value = 90,
            status = "success",
            title = paste("Saving Metadata")
          )
          print(2)
          #Create random deployid
          deployidcreate = random_id(n = 1,bytes = 16)
          deployid(deployidcreate)
          print(3)
          ##Update tables
          #Load tables
          updateproclogs = processinglogs()
          deployadd = deploylogs()
          qctable = qc_config()
          stationupdate = stations()
          
          updateproclogs$DeployID[which(updateproclogs$StationID == input$procstationname & 
                                          updateproclogs$ModelID == input$procmodel & 
                                          is.na(updateproclogs$Processed))] = deployid()
          print(4)
          #Update the depths table processed date
          if (turnoffprocessupdate == FALSE){
            updateproclogs$Processed[which(updateproclogs$StationID == input$procstationname &
                                          updateproclogs$ModelID == input$procmodel & is.na(updateproclogs$Processed))] = Sys.Date()
          }
          processinglogs(updateproclogs)
          print(5)
          #Update deploy table
          deployaddrows = NULL
          for (k in names(compiledata)){
            print("x")
            selectunit = unique(qctable$Units[which(qctable$AppID == input$procwaterbody & qctable$Logger_Type == k)])
            selectunit = selectunit[!is.na(selectunit)]
            
            deployaddrow = data.frame(
              "DeployID" = deployidcreate,
              "ModelID" = input$procmodel,
              "Logger_Type" = k,
              "Lat" = storelat(),
              "Lon" = storelon(),
              "StartDateTimeRecord" = as.character(NA),
              "EndDateTimeRecord" = as.character(NA),
              "StartDateTimeValid" = as.character(NA),
              "EndDateTimeValid" = as.character(NA),
              "Units" = selectunit,
              "Z" = paste(sort(unique(depthsdata$Z)),collapse = ","),
              "Logger_Count" = length(datapaths),
              "Deployment_Count" = storedepcount(),
              "ProcessedDate" = as.character(Sys.Date()),
              "Processedby" = storeuser(),
              stringsAsFactors = FALSE
            )
            print("y")
            deployaddrows = rbind(deployaddrows,deployaddrow)
          }
          deployadd = rbind(deployadd,deployaddrows)
          print(6)
          deploylogs(deployadd)
          
          #Update stations table
          stationupdate$Lat[which(stationupdate$StationID == input$procstationname)] = storelat()
          stationupdate$Lon[which(stationupdate$StationID == input$procstationname)] = storelon()
          
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
    
    finaldata(NULL)
  }
)

#Re-enable disabled Processing button upon uploading new data
observeEvent(
  input$dataupload,{
    toggleState("processing")
  }
)

#Extract logger types
qcloggertypes = reactive({
  qcloggers = VisQCdata()
  qcloggers = names(qcloggers)
  
  return(qcloggers)
})

#Render Data Table for Review of Processed Data
output$dataoutput = renderDT(
  options = list(
    lengthChange = FALSE
  ),
  extensions = 'Responsive',{
    
    qcdatadisplay = VisQCdata()
    datatypeselect = head(qcdatadisplay[[input$prevloggerchoices]])
    
    return(datatypeselect[,c(1:4,15:18)])
  }
)