#not in operator definition
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

readhobodata=function(hobodatapath){
  
 if(class(try(read.table(hobodatapath,header = TRUE,stringsAsFactors = FALSE,sep = ","),silent = TRUE))
    =="try-error"){
   tempdata=read.table(hobodatapath,header = TRUE,stringsAsFactors = FALSE,skip = 1,sep=",")
 }else{
   tempdata=read.table(hobodatapath,header = TRUE,stringsAsFactors = FALSE,sep=",")
 }
 return(tempdata)
   
}

#tests for the correct temperature scale and time zone
formattests=function(datapaths,inputs){
  #Check for GMT time zone and Temperature units for temperature logger processing--------
  if (inputs=="Temperature"){
    for (i in datapaths){
      GMTtempcsv=readhobodata(i)
      GMTcol=colnames(GMTtempcsv[2])
      GMTtest=grepl(pattern="Date.Time..GMT.00.0",GMTcol)
      if("F" %in% colnames(GMTtempcsv[3])){
        warningcsvtemp=i
        Temperror=paste0(warningcsvtemp$name, "'s temperature scale is not set to Celsius")
        #Sends alert if the incorrect temperature scale has been used
        sendSweetAlert(
          session,
          title = "Incorrect Temperature Scale",
          text = Temperror,
          type = "error"
        )
      }
      if(GMTtest==FALSE){
        warningcsv=filetable[filetable$datapath==i,]
        GMTerror=paste0(warningcsv$name,"'s time zone is not set to UTC/GMT")
        sendSweetAlert(
          session,
          title = "Incorrect Time Zone",
          text = GMTerror,
          type = "error"
        )
      }
    }
  }
}

#Reformat HOBO data into format for ContDataQC
formatforQC=function(filedata,inputs){
  message("Start formatforQC")
  filedata=as.data.frame(filedata)
  message(nrow(filedata))
  #Processes DO and Temp
  inputname=as.character(filedata$name)
  inputname=as.character(gsub(".csv","",inputname))
  inputname=as.character(gsub(".TXT","",inputname))
  inputname=as.character(gsub(".txt","",inputname))
  message(inputname)
  
  if (inputs=="Temperature"){
    message("Start Temperature Data Formatting")
    print(as.character(filedata$datapath))
    
    importtempcsv=readhobodata(as.character(filedata$datapath))
    
    if(all(!is.na(as.POSIXct(importtempcsv[,2],format="%m/%d/%y %r",tz="UTC")))){
      importtempcsv[,2]=as.POSIXct(importtempcsv[,2],format="%m/%d/%y %r",tz="UTC")
    }else{
      importtempcsv[,2]=as.POSIXct(importtempcsv[,2],format="%m/%d/%Y %H:%M",tz="UTC")
    }
    
    outputcsv=data.frame("WaterID"=importtempcsv[,1],"Date Time"=importtempcsv[,2],"Water Temp C"=importtempcsv[,3])
    outputcsv["SiteID"]=as.character(inputname)
    
    #Extracts first and last dates for adding to the output csv name
    maxdate=max(as.Date(importtempcsv[,2]))
    maxdatetxt=gsub("-","",as.character(maxdate))
    mindate=min(as.Date(importtempcsv[,2]))
    mindatetxt=gsub("-","",as.character(mindate))
    
    SNdatesrow=data.frame("SN"=as.character(inputname),"Start_Date"=mindate,"End_Date"=maxdate)
    
    
    #Pastes together the appropriate naming convention
    outputcsvname=paste0(inputname,"_Water_",mindatetxt,"_",maxdatetxt,".csv")
    
    dir.create("Temp/processing",showWarnings = FALSE)
    
    procoutput=paste0("Temp/processing/",outputcsvname)
    # procoutput=paste0("D:/Datasets/Water_Temperature/Reevaluation/Peltier/2017/Temp/processing/",outputcsvname)
    message(paste("Saving",outputcsvname))
    #Writes the csv
    write.csv(outputcsv,procoutput,fileEncoding = "ISO-8859-1",row.names=FALSE)
  }else if (inputs=="Dissolved Oxygen"){
    
    message("Start DO Formatting")
    # i="D:/Datasets/DO/Continuous/Greenwood/2019/Cat/533375.TXT"
    # i="D:/Datasets/DO/Continuous/Greenwood/2019/All/742710.csv"
    
    importtempcsv=read.table(filedata$datapath,sep = ",",header = TRUE,stringsAsFactors = FALSE)
    if (ncol(importtempcsv)==1){
      importtempcsv=read.table(filedata$datapath,skip = 7,sep = ",",header = TRUE,stringsAsFactors = FALSE)
    }
    importtempcsv=as.data.frame(importtempcsv[-1,])
    importtempcsv[,2]=as.POSIXct(importtempcsv[,2],tz="UTC")
    
    #Create DO data frame
    outputcsvDO=data.frame("Date Time"=importtempcsv[,2],"DO.mg.L"=importtempcsv[,6])
    outputcsvDO["SiteID"]=as.character(inputname)
    #Create Temp data frame
    outputcsvTemp=data.frame("Date Time"=importtempcsv[,2],"Water Temp C"=importtempcsv[,5])
    outputcsvTemp["SiteID"]=as.character(inputname)
    
    #Extracts first and last dates for adding to the output csv name
    maxdate=max(as.Date(importtempcsv[,2]))
    maxdatetxt=gsub("-","",as.character(maxdate))
    mindate=min(as.Date(importtempcsv[,2]))
    mindatetxt=gsub("-","",as.character(mindate))
    
    SNdatesrow=data.frame("SN"=as.character(inputname),"Start_Date"=mindate,"End_Date"=maxdate)
    
    
    #CSV names
    outputcsvnamedo=paste0(inputname,"_Water_",mindatetxt,"_",maxdatetxt,".csv")
    outputcsvnametemp=paste0(inputname,"_Water_",mindatetxt,"_",maxdatetxt,".csv")
    
    #paths for saving csv files
    dir.create("DO/DO/processing/",showWarnings = FALSE)
    dir.create("DO/Temp/processing/",showWarnings = FALSE)
    
    procoutputDO=paste0("DO/DO/processing/",outputcsvnamedo)
    procoutputTemp=paste0("DO/Temp/processing/",outputcsvnametemp)
    
    #Write files for both DO and Temp data
    write.csv(outputcsvDO,procoutputDO,fileEncoding = "ISO-8859-1",row.names=FALSE)
    write.csv(outputcsvTemp,procoutputTemp,fileEncoding = "ISO-8859-1",row.names=FALSE)
  }
  return(SNdatesrow)
}

#Run automatic ContDataQC Processes
QCProcess=function(SNdates,inputs){
  SiteIDs=SNdates$SN
  for (i in SiteIDs){
    message(paste("Start QC: ",i))
    qcdate=SNdates[as.character(SNdates$SN)==i,]
    if (inputs=="Temperature"){
      #Temperature Only
      ContDataQC(
        fun.myData.Operation = "QCRaw",
        fun.myDir.import = "Temp/processing/",
        fun.myDir.export = "Temp/qc/",
        fun.myData.Type = "Water",
        fun.myData.DateRange.Start = as.character(as.Date(qcdate$Start_Date,origin="1970-01-01")),
        fun.myData.DateRange.End = as.character(as.Date(qcdate$End_Date,origin="1970-01-01")),
        fun.myData.SiteID = i,
        fun.CreateReport = FALSE,
        fun.myConfig = configfilepath()
      )
      
    }else if (inputs=="Dissolved Oxygen"){
      #DO
      ContDataQC(
        fun.myData.Operation = "QCRaw",
        fun.myDir.import = "DO/DO/processing/",
        fun.myDir.export = "DO/DO/qc/",
        fun.myData.Type = "Water",
        fun.myData.DateRange.Start = as.character(as.Date(qcdate$Start_Date,origin="1970-01-01")),
        fun.myData.DateRange.End = as.character(as.Date(qcdate$End_Date,origin="1970-01-01")),
        fun.myData.SiteID = i,
        fun.CreateReport = FALSE,
        fun.myConfig = configfilepath()
      )
      #Temperature
      ContDataQC(
        fun.myData.Operation = "QCRaw",
        fun.myDir.import = "DO/Temp/processing/",
        fun.myDir.export = "DO/Temp/qc/",
        fun.myData.Type = "Water",
        fun.myData.DateRange.Start = as.character(as.Date(qcdate$Start_Date,origin="1970-01-01")),
        fun.myData.DateRange.End = as.character(as.Date(qcdate$End_Date,origin="1970-01-01")),
        fun.myData.SiteID = i,
        fun.CreateReport = FALSE,
        fun.myConfig = configfilepath()
      )
    }
  }
}

#Add Depth Data and organize files into updated format
depthorganize=function(depthstable,inputs,chainname){
  
  if (inputs=="Temperature"){
    qcfiles=list.files("Temp/qc/",
                       pattern = ".csv")
    combined=NULL
    for (i in qcfiles){
      message(paste("Reorder",i))
      combimport=paste0("Temp/qc/",i)
      imp=read.csv(combimport, header=TRUE)
      depthset=depthstable$Depth[as.character(depthstable$Serial_Number)==unique(as.character(imp$SiteID))]
      
      imp["Depth"]=depthset
      combined=rbind(combined,imp)
    }
    
    #Create Data Frame----------
    Temp_Log=data.frame("LakeId"=lakeid(),"Serial_Number"=as.character(combined$SiteID),"Date_Time"=combined$Date.Time,
                        "Water_Temp_C"=combined$Water.Temp.C,"Depth_m"=combined$Depth,
                        "FlagG"=combined$Flag.Gross.Water.Temp.C,"FlagS"=combined$Flag.Spike.Water.Temp.C,
                        "FlagR"=combined$Flag.RoC.Water.Temp.C,"FlagF"=combined$Flag.Flat.Water.Temp.C,"Chain"=chainname,
                        stringsAsFactors = FALSE)
    
    Temp_Log["FlagV"]="P"
    
    Temp_Log=Temp_Log[,c(1,2,3,4,5,6,7,8,9,11,10)]
    
    
    Temp_Log=Temp_Log[Temp_Log$FlagS!="X",]
    
    Finaldata=Temp_Log
    
    #Delete Temp Files
    unlink("Temp/processing/*.csv")
    unlink("Temp/qc/*.csv")
    unlink("Temp/qc/*.html")
    unlink("*.tab")
  }
  else if (inputs=="Dissolved Oxygen"){
    DOqcfiles=list.files("DO/DO/qc/",
                         pattern = ".csv")
    
    # DOqcfiles=list.files("C:/Projects/Shiny_App_Development/Data_Processing/Logger_Processing/3.0/DO/DO/qc/",
    #                      pattern = ".csv")
    
    Tempqcfiles=list.files("DO/Temp/qc",
                           pattern = ".csv")

    # Tempqcfiles=list.files("C:/Projects/Shiny_App_Development/Data_Processing/Logger_Processing/3.0/DO/Temp/qc/",
    #                        pattern = ".csv")
    
    DOcombined=NULL
    for (i in DOqcfiles){
      message(paste("Reorder DO",i))
      DOcombimport=paste0("DO/DO/qc/","/",i)
      # DOcombimport=paste0("C:/Projects/Shiny_App_Development/Data_Processing/Logger_Processing/3.0/DO/DO/qc/","/",i)
      
      DOimp=read.csv(DOcombimport, header=TRUE)
      DOimp["FullID"]=gsub(".*QC_\\s*|_Water.*","",i)
      message(gsub(".*QC_\\s*|_Water.*","",i))
      DOdepthset=depthstable$Depth[as.character(depthstable$Serial_Number)==unique(as.character(DOimp$FullID))]
      DOimp["Depth"]=as.numeric(DOdepthset)
      DOcombined=rbind(DOcombined,DOimp)
    }
    unique(DOcombined$FullID)
    Tempcombined=NULL
    for (i in Tempqcfiles){
      message(paste("Reorder Temperature",i))
      Tempcombimport=paste0("DO/Temp/qc","/",i)
      # Tempcombimport=paste0("C:/Projects/Shiny_App_Development/Data_Processing/Logger_Processing/3.0/DO/Temp/qc/","/",i)
      Tempimp=read.csv(Tempcombimport, header=TRUE)
      Tempimp["FullID"]=gsub(".*QC_\\s*|_Water.*","",i)
      message(gsub(".*QC_\\s*|_Water.*","",i))
      Tempcombined=rbind(Tempcombined,Tempimp)
    }
    
    DO_Log=data.frame("LakeId"=lakeid(),"Serial_Number"=as.character(DOcombined$FullID),"Date_Time"=DOcombined$Date.Time,
                      "DO"=DOcombined$DO.mg.L,"Depth_m"=DOcombined$Depth,
                      "DO_FlagG"=DOcombined$Flag.Gross.DO.mg.L,"DO_FlagS"=DOcombined$Flag.Spike.DO.mg.L,
                      "DO_FlagR"=DOcombined$Flag.RoC.DO.mg.L,"DO_FlagF"=DOcombined$Flag.Flat.DO.mg.L,stringsAsFactors = FALSE)
    message("DO data reorganized")
    DO_Log["DO_FlagV"]="P"
    DO_Log$DO_FlagV=as.character(DO_Log$DO_FlagV)
    # DO_Log=DO_Log[DO_Log$DO_FlagS!="F",]
    DO_Log=DO_Log[DO_Log$DO_FlagS!="X",]
    
    DOTemp_Log=data.frame("Serial_Number"=as.character(Tempcombined$FullID),"Date_Time"=Tempcombined$Date.Time,
                          "Water.Temp.C"=Tempcombined$Water.Temp.C,
                          "Temp_FlagG"=Tempcombined$Flag.Gross.Water.Temp.C,"Temp_FlagS"=Tempcombined$Flag.Spike.Water.Temp.C,
                          "Temp_FlagR"=Tempcombined$Flag.RoC.Water.Temp.C,"Temp_FlagF"=Tempcombined$Flag.Flat.Water.Temp.C,
                          stringsAsFactors = FALSE)
    message("Temperature data reorganized")
    DOTemp_Log["Temp_FlagV"]="P"
    message("FlagV field added")
    # DOTemp_Log$Temp_FlagV=as.character(DOTemp_Log$Temp_FlagV)
    
    DOdataall=left_join(DO_Log,DOTemp_Log,by=c("Serial_Number","Date_Time"))
    message("Data types joined")
    
    Finaldata=data.frame("LakeId"=DOdataall$LakeId,"Serial_Number"=as.character(DOdataall$Serial_Number),"Date_Time"=DOdataall$Date_Time,
                         "DO"=DOdataall$DO,"Water_Temp_C"=DOdataall$Water.Temp.C,
                         "Depth_m"=DOdataall$Depth_m,"DO_FlagG"=DOdataall$DO_FlagG,"DO_FlagS"=DOdataall$DO_FlagS,
                         "DO_FlagR"=DOdataall$DO_FlagR,"DO_FlagF"=DOdataall$DO_FlagF,"DO_FlagV"=DOdataall$DO_FlagV,
                         "Temp_FlagG"=DOdataall$Temp_FlagG,"Temp_FlagS"=DOdataall$Temp_FlagS,"Temp_FlagR"=DOdataall$Temp_FlagR,
                         "Temp_FlagF"=DOdataall$Temp_FlagF,"Temp_FlagV"=DOdataall$Temp_FlagV,stringsAsFactors = FALSE)
    unlink("DO/DO/processing/*.csv")
    unlink("DO/DO/qc/*.csv")
    unlink("DO/DO/qc/*.html")
    unlink("DO/Temp/processing/*.csv")
    unlink("DO/Temp/qc/*.csv")
    unlink("DO/Temp/qc/*.html")
    unlink("*.tab")
  }
  return(Finaldata)
}

#Create Reactive Value for VisQCdata step
VisQCdata=reactiveVal()
VisQCvalues=reactiveVal()

#Process and QC data----
#Observes the Processing button and begins reformating and QCing the data
observeEvent(
  input$processing,{
    
    #Ensures that data have been uploaded
    if(length(input$dataupload)>0){
      
      #Table with information about the uploaded data
      filetable=input$dataupload
      #Temp paths to the uploaded data
      datapaths=filetable$datapath
      #Logger lookup table for associating loggers to depths
      depthstable=depthsfile()
      #Selects lookup table rows for selected lake and that have not been processed
      depthstable=depthstable[formatlakename(depthstable$Lake)==lakeinput() & is.na(depthstable$Processed),]
      
      #Test for correct temperature scale and timezone
      formattests(datapaths = datapaths,
                  inputs = input$Type)

      updateProgressBar(
        session,
        "processprogress",
        value = 5,
        status = "success")
      #Fix Table Organization and rename csv---------
      #Iterates through the uploaded files and formats them into the ContDataQC format
      SNdates=NULL
      for (j in 1:nrow(filetable)){
        QCformat=formatforQC(
          filedata = filetable[j,],
          inputs = input$Type)
        
        SNdates=rbind(SNdates,QCformat)
      }
      
      updateProgressBar(
        session,
        "processprogress",
        value=15,
        status = "success"
      )
      
      #QC Data------------
      #ContDataQC process
      QCProcess(SNdates = SNdates,
                inputs = input$Type)
      
      updateProgressBar(
        session,
        "processprogress",
        value = 65,
        status = "success"
      )
      
      finalprocess=depthorganize(
        depthstable = depthstable,
        inputs = input$Type,
        chainname = input$chain
      )

      updateProgressBar(
        session,
        "processprogress",
        value = 100,
        status = "success"
      )
      #Write to reactive data
      VisQCdata(finalprocess)
      VisQCvalues(finalprocess)
    }else{
      sendSweetAlert(
        session,
        title = "Missing Logger Data",
        text = "Please upload logger data",
        type = "error"
      )
    }
  })

#Render Data Table for Review of Processed Data
output$dataoutput=renderDT(
  options=list(
    lengthChange = FALSE
  ),
  extensions = 'Responsive',
  {
    qcdatadisplay=VisQCdata()
    return(qcdatadisplay)
  })