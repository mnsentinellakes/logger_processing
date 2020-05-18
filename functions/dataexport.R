#Export Data----

exportdata=reactiveVal()
# exportdata=VisQCdata()
#Process data for download-----------
observeEvent(
  input$processexport,{
    if(input$exporttype=="All Data"){
      updateProgressBar(
        session,
        "exportprogress",
        value = 50,
        status = "primary"
      )
      exportdata=VisQCdata()
      
      updateProgressBar(
        session,
        "exportprogress",
        value = 100,
        status = "primary"
      )
    }else if(input$exporttype=="Remove Failed Visual QC"){
      updateProgressBar(
        session,
        "exportprogress",
        value = 50,
        status = "primary"
      )
      exportdata=VisQCdata()
      if (input$Type == "Temperature"){
        exportdata=exportdata[which(exportdata$FlagV=="P"),]
      }else if (input$Type =="Dissolved Oxygen"){
        exportdata=exportdata[which(exportdata$DO_FlagV=="P" & exportdata$Temp_FlagV=="P"),]
      }
      updateProgressBar(
        session,
        "exportprogress",
        value = 100,
        status = "primary"
      )
    }else if(input$exporttype=="Daily Summary"){
      updateProgressBar(
        session,
        "exportprogress",
        value = 2,
        status = "primary"
      )
      exportdata=VisQCdata()
      
      if (input$Type=="Temperature"){
        
        exportdata=exportdata[exportdata$FlagV=="P",]
        exportdata$Depth_m=as.numeric(exportdata$Depth_m)
        
        #Remove unneeded fields-------
        
        exportdata$LakeId=NULL
        exportdata$FlagG=NULL
        exportdata$FlagS=NULL
        exportdata$FlagR=NULL
        exportdata$FlagF=NULL
        exportdata$FlagV=NULL
        message("Removed unnecessary fields")
        
        updateProgressBar(
          session,
          "exportprogress",
          value = 5,
          status = "primary"
        )
        
        #Get max and min depths and create list of depths in between-----
        MaxDepth=round(max(exportdata$Depth_m),digits=0)
        MinDepth=round(min(exportdata$Depth_m),digits=0)
        Standard_Depths=c(MinDepth:MaxDepth)
        laketempdepths=sort(unique(exportdata$Depth_m))
        message("Standardized depths")
        
        updateProgressBar(
          session,
          "exportprogress",
          value = 10,
          status = "primary"
        )
        dailydepths=NULL
        #Temperature stats by day-----
        for (i in laketempdepths){
          depth=exportdata[exportdata$Depth_m==i,]
          message(paste("Summarizing Depth",i))
          depth.xts=as.xts(as.numeric(as.numeric(depth[,3])),order.by=as.POSIXct(depth[,2],format='%Y-%m-%d %H:%M:%S',tz="UTC"))
          depth.mean.xts=apply.daily(depth.xts,mean)
          depth.max.xts=apply.daily(depth.xts,max)
          depth.min.xts=apply.daily(depth.xts,min)
          depth.sd.xts=apply.daily(depth.xts,sd)
          depthmean=data.frame("datetime"=index(depth.mean.xts),"Depth"=i,"Mean_Temp"=coredata(depth.mean.xts))
          depthmax=data.frame("datetime"=index(depth.max.xts),"Depth"=i,"Max_Temp"=coredata(depth.max.xts))
          depthmin=data.frame("datetime"=index(depth.min.xts),"Depth"=i,"Min_Temp"=coredata(depth.min.xts))
          depthsd=data.frame("datetime"=index(depth.sd.xts),"Depth"=i,"Stand_Dev"=coredata(depth.sd.xts))
          
          depthday=left_join(depthmean,depthmax,by=c("datetime","Depth"))
          depthday=left_join(depthday,depthmin,by=c("datetime","Depth"))
          depthday=left_join(depthday,depthsd,by=c("datetime","Depth"))
          
          dailydepths=rbind(dailydepths,depthday)
          
          updateProgressBar(
            session,
            "exportprogress",
            value = 60/length(laketempdepths),
            status = "info"
          )
        }
        
        dailydepths$datetime=as.Date(dailydepths$datetime)
        
        #Get a list of dates-----
        meandays=unique(dailydepths$datetime)
        Standardized.temp.data=NULL
        for (j in meandays){
          Daysubset=dailydepths[dailydepths$datetime==j,]

          if (nrow(Daysubset)>1){
            inter.mean=approx(Daysubset$Depth,Daysubset$Mean_Temp,method="linear",xout=Standard_Depths)
            inter.max=approx(Daysubset$Depth,Daysubset$Max_Temp,method="linear",xout=Standard_Depths)
            inter.min=approx(Daysubset$Depth,Daysubset$Min_Temp,method="linear",xout=Standard_Depths)
            inter.sd=approx(Daysubset$Depth,Daysubset$Stand_Dev,method="linear",xout=Standard_Depths)
            Standardized.row.mean=data.frame("Date"=unique(Daysubset$datetime),"Depth"=inter.mean$x,"Temperature"=round(inter.mean$y,digits = 3))
            Standardized.row.max=data.frame("Date"=unique(Daysubset$datetime),"Depth"=inter.max$x,"Temperature_Max"=round(inter.max$y,digits = 3))
            Standardized.row.min=data.frame("Date"=unique(Daysubset$datetime),"Depth"=inter.min$x,"Temperature_Min"=round(inter.min$y,digits = 3))
            Standardized.row.sd=data.frame("Date"=unique(Daysubset$datetime),"Depth"=inter.sd$x,"Standard_Deviation"=round(inter.sd$y,digits = 3))
            Standardized.row=left_join(Standardized.row.mean,Standardized.row.max,by="Date")
            Standardized.row=left_join(Standardized.row,Standardized.row.min,by="Date")
            Standardized.row=left_join(Standardized.row,Standardized.row.sd,by="Date")
            Standardized.row$LakeId=lakeid()
            Standardized.row=Standardized.row[,c(7,1,2,3,4,5,6)]
            message(paste(j,"interpolated"))
          }else{
            Standardized.row=data.frame("LakeId"=lakeid(),"Date"=unique(Daysubset$datetime),"Depth"=round(Daysubset$Depth,digits=0),
                                        "Temperature"=Daysubset$Mean_Temp,"Temperature_Max"=Daysubset$Max_Temp,
                                        "Temperature_Min"=Daysubset$Min_Temp,"Standard_Deviation"=Daysubset$Stand_Dev)
          }
          Standardized.temp.data=rbind(Standardized.temp.data,Standardized.row)
        }
        Standardized.temp.data$Date=as.Date(Standardized.temp.data$Date)
        Standardized.temp.data=Standardized.temp.data[order(Standardized.temp.data$Date),]
        exportdata=Standardized.temp.data
        updateProgressBar(
          session,
          "exportprogress",
          value = 100,
          status = "success"
        )
      }else if (input$Type=="Dissolved Oxygen"){
        
        # exportdata=read.csv("C:/Users/timarti1/Downloads/Greenwood_DO_2019_10_15.csv",stringsAsFactors = FALSE)
        exportdata=exportdata[exportdata$DO_FlagV=="P",]
        exportdata=exportdata[exportdata$Temp_FlagV=="P",]
        # exportdata=exportdata[!is.na(exportdata$Date_Time),]
        exportdata$Depth_m=as.numeric(exportdata$Depth_m)
        
        #Remove unneeded fields-------
        # exportdata$DOWLKNUM=NULL
        exportdata$LakeId=NULL
        exportdata$DO_FlagG=NULL
        exportdata$DO_FlagS=NULL
        exportdata$DO_FlagR=NULL
        exportdata$DO_FlagF=NULL
        exportdata$DO_FlagV=NULL
        exportdata$Temp_FlagG=NULL
        exportdata$Temp_FlagS=NULL
        exportdata$Temp_FlagR=NULL
        exportdata$Temp_FlagF=NULL
        exportdata$Temp_FlagV=NULL
        message("Removed unnecessary fields")
        
        updateProgressBar(
          session,
          "exportprogress",
          value = 5,
          status = "primary"
        )
        
        #Get max and min depths and create list of depths in between-----
        MaxDepth=round(max(exportdata$Depth_m),digits=0)
        MinDepth=round(min(exportdata$Depth_m),digits=0)
        Standard_Depths=c(MinDepth:MaxDepth)
        laketempdepths=sort(unique(exportdata$Depth_m))
        message("Standardized depths")
        
        updateProgressBar(
          session,
          "exportprogress",
          value = 10,
          status = "primary"
        )
        DOdailydepths=NULL
        Tempdailydepths=NULL
        
        #Summarize temperature and DO by day-----
        for (i in laketempdepths){
          message(paste("Summarizing depth",i))
          depth=exportdata[exportdata$Depth_m==i,]
          DOdepth.xts=as.xts(as.numeric(depth[,3]),order.by=as.POSIXct(depth[,2],format='%Y-%m-%d %H:%M',tz="UTC"))
          DOdepth.mean.xts=apply.daily(DOdepth.xts,mean)
          DOdepth.max.xts=apply.daily(DOdepth.xts,max)
          DOdepth.min.xts=apply.daily(DOdepth.xts,min)
          DOdepth.sd.xts=apply.daily(DOdepth.xts,sd)
          DOdepthmean=data.frame("datetime"=index(DOdepth.mean.xts),"Depth"=i,"Mean_DO"=coredata(DOdepth.mean.xts))
          DOdepthmax=data.frame("datetime"=index(DOdepth.max.xts),"Depth"=i,"Max_DO"=coredata(DOdepth.max.xts))
          DOdepthmin=data.frame("datetime"=index(DOdepth.min.xts),"Depth"=i,"Min_DO"=coredata(DOdepth.min.xts))
          DOdepthsd=data.frame("datetime"=index(DOdepth.sd.xts),"Depth"=i,"Stand_Dev_DO"=coredata(DOdepth.sd.xts))
          
          DOdepthday=left_join(DOdepthmean,DOdepthmax,by=c("datetime","Depth"))
          DOdepthday=left_join(DOdepthday,DOdepthmin,by=c("datetime","Depth"))
          DOdepthday=left_join(DOdepthday,DOdepthsd,by=c("datetime","Depth"))
          
          DOdailydepths=rbind(DOdailydepths,DOdepthday)
          
          Tempdepth.xts=as.xts(as.numeric(depth[,4]),order.by=as.POSIXct(depth[,2],format='%Y-%m-%d %H:%M',tz="UTC"))
          Tempdepth.mean.xts=apply.daily(Tempdepth.xts,mean)
          Tempdepth.max.xts=apply.daily(Tempdepth.xts,max)
          Tempdepth.min.xts=apply.daily(Tempdepth.xts,min)
          Tempdepth.sd.xts=apply.daily(Tempdepth.xts,sd)
          Tempdepthmean=data.frame("datetime"=index(Tempdepth.mean.xts),"Depth"=i,"Mean_Temp"=coredata(Tempdepth.mean.xts))
          Tempdepthmax=data.frame("datetime"=index(Tempdepth.max.xts),"Depth"=i,"Max_Temp"=coredata(Tempdepth.max.xts))
          Tempdepthmin=data.frame("datetime"=index(Tempdepth.min.xts),"Depth"=i,"Min_Temp"=coredata(Tempdepth.min.xts))
          Tempdepthsd=data.frame("datetime"=index(Tempdepth.sd.xts),"Depth"=i,"Stand_Dev_Temp"=coredata(Tempdepth.sd.xts))
          
          Tempdepthday=left_join(Tempdepthmean,Tempdepthmax,by=c("datetime","Depth"))
          Tempdepthday=left_join(Tempdepthday,Tempdepthmin,by=c("datetime","Depth"))
          Tempdepthday=left_join(Tempdepthday,Tempdepthsd,by=c("datetime","Depth"))
          
          Tempdailydepths=rbind(Tempdailydepths,Tempdepthday)
          
          updateProgressBar(
            session,
            "exportprogress",
            value = 60/length(laketempdepths),
            status = "info"
          )
        }
        
        
        dailydepths=left_join(DOdailydepths,Tempdailydepths,by=c("datetime","Depth"))
        
        #Get a list of dates-----
        meandays=unique(dailydepths$datetime)
        Standardized.temp.data=NULL
        
        for (j in meandays){
          Daysubset=dailydepths[dailydepths$datetime==j,]
          
          if (nrow(Daysubset)>1){
            # DOStandardized.row=NULL
            # TempStandardized.row=NULL
            
            DOinter.mean=approx(Daysubset$Depth,Daysubset$Mean_DO,method="linear",xout=Standard_Depths)
            DOinter.max=approx(Daysubset$Depth,Daysubset$Max_DO,method="linear",xout=Standard_Depths)
            DOinter.min=approx(Daysubset$Depth,Daysubset$Min_DO,method="linear",xout=Standard_Depths)
            
            if(nrow(Daysubset[which(!is.na(Daysubset$Stand_Dev_DO)),])>2){
              DOinter.sd=approx(Daysubset$Depth,Daysubset$Stand_Dev_DO,method="linear",xout=Standard_Depths)
              DOsd=DOinter.sd$y
            }else{
              DOsd=NA
            }
            Tempinter.mean=approx(Daysubset$Depth,Daysubset$Mean_Temp,method="linear",xout=Standard_Depths)
            Tempinter.max=approx(Daysubset$Depth,Daysubset$Max_Temp,method="linear",xout=Standard_Depths)
            Tempinter.min=approx(Daysubset$Depth,Daysubset$Min_Temp,method="linear",xout=Standard_Depths)
            if(nrow(Daysubset[which(!is.na(Daysubset$Stand_Dev_Temp)),])>2){
              Tempinter.sd=approx(Daysubset$Depth,Daysubset$Stand_Dev_Temp,method="linear",xout=Standard_Depths) 
              Tempsd=Tempinter.sd$y
            }else{
              Tempsd=NA
            }
            Standardized.row=data.frame("LakeId"=lakeid(),"Date"=unique(Daysubset$datetime),"Depth"=DOinter.mean$x,"DO"=round(DOinter.mean$y,digits = 3),
                                        "DO_Max"=round(DOinter.max$y,digits = 3),"DO_Min"=round(DOinter.min$y,digits = 3),"DO_Standard_Deviation"=round(DOsd,digits = 3),
                                        "Temperature"=round(Tempinter.mean$y,digits = 3),"Temperature_Max"=round(Tempinter.max$y,digits = 3),
                                        "Temperature_Min"=round(Tempinter.min$y,digits = 3),"Temperature_Standard_Deviation"=round(Tempsd,digits = 3))
            message(paste(j,"interpolated"))
          }else{
            Standardized.row=data.frame("LakeId"=lakeid(),"Date"=unique(Daysubset$datetime),"Depth"=round(Daysubset$Depth,digits=0),
                                        "DO"=Daysubset$Mean_DO,"DO_Max"=Daysubset$Max_DO,"DO_Min"=Daysubset$Min_DO,
                                        "DO_Standard_Devation"=Daysubset$Stand_Dev_DO,
                                        "Temperature"=Daysubset$Mean_Temp,"Temperature_Max"=Daysubset$Max_Temp,
                                        "Temperature_Min"=Daysubset$Min_Temp,
                                        "Temperature_Standard_Deviation"=Daysubset$Stand_Dev_Temp)
          }
          Standardized.temp.data=rbind(Standardized.temp.data,Standardized.row)
        }
        Standardized.temp.data$Date=as.Date(Standardized.temp.data$Date)
        Standardized.temp.data=Standardized.temp.data[order(Standardized.temp.data$Date),]
        exportdata=Standardized.temp.data
        updateProgressBar(
          session,
          "exportprogress",
          value = 100,
          status = "success"
        )
      }
    }
    exportdata(exportdata)
  })


output$exportpreview=renderDT(
  options=list(
    lengthChange = FALSE
  ),
  rownames = FALSE,
  extensions = 'Responsive',{
    datapreview=exportdata()
    datapreview
  })


#Download Data----
output$download <- downloadHandler(
  filename = function() { 
    if (input$Type=="Temperature"){
      paste0(lakeinput(),"_Temperature_",gsub("-","_",as.character(Sys.Date())),".csv")
    }else if (input$Type=="Dissolved Oxygen"){
      paste0(lakeinput(),"_DO_",gsub("-","_",as.character(Sys.Date())),".csv")
    }
  },
  content = function(file) {
    dataforexport=exportdata()
    write.csv(dataforexport,file,row.names=F)
  })
