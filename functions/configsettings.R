#Load Code
#default config file
configfileselect=reactiveVal()
configfileselect("default")

observe({
  source(paste0("config/",configfileselect(),".R"))
})
  
# update configfileselect() and textInputs when input$loadconfig Actionbttn is clicked
observeEvent(input$loadconfig,{
  configfileselect(input$configfile)
  
  source(paste0("config/",configfileselect(),".R"))
  
  updateTextInput(
    session = session,
    inputId = "tempgrossmaxfail",
    value = ContData.env$myThresh.Gross.Fail.Hi.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempgrossminfail",
    value = ContData.env$myThresh.Gross.Fail.Lo.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempgrossmaxsusp",
    value = ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempgrossminsusp",
    value = ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempspikemaxfail",
    value = ContData.env$myThresh.Spike.Hi.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempspikeminfail",
    value = ContData.env$myThresh.Spike.Lo.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "temprocsd",
    value = ContData.env$myThresh.RoC.SD.number.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "temprocperiod",
    value = ContData.env$myThresh.RoC.SD.period.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempflathi",
    value = ContData.env$myThresh.Flat.Hi.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempflatlow",
    value = ContData.env$myThresh.Flat.Lo.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "tempflattol",
    value = ContData.env$myThresh.Flat.Tolerance.WaterTemp
  )
  
  updateTextInput(
    session = session,
    inputId = "dogrossmaxfail",
    value = ContData.env$myThresh.Gross.Fail.Hi.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dogrossminfail",
    value = ContData.env$myThresh.Gross.Suspect.Hi.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dogrossmaxsusp",
    value = ContData.env$myThresh.Gross.Suspect.Hi.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dogrossminsusp",
    value = ContData.env$myThresh.Gross.Suspect.Lo.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dospikemaxfail",
    value = ContData.env$myThresh.Spike.Hi.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dospikeminfail",
    value = ContData.env$myThresh.Spike.Lo.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dorocsd",
    value = ContData.env$myThresh.RoC.SD.number.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "dorocperiod",
    value = ContData.env$myThresh.RoC.SD.period.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "doflathi",
    value = ContData.env$myThresh.Flat.Hi.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "doflatlow",
    value = ContData.env$myThresh.Flat.Lo.DO
  )
  
  updateTextInput(
    session = session,
    inputId = "doflattol",
    value = ContData.env$myThresh.Flat.Tolerance.DO
  )
})

#VERIFY THIS
#Reset the default config file to original values
observeEvent(input$restoredefault,{
  file.copy(
    from = "config/default_orig.R",
    to = "config/default.R",
    overwrite = TRUE
  )
  configfileselect("default")
})

configfilepath = reactive({
  configpath = paste0("config/",configfileselect(),".R")
})

#Save Code
#build and save config file
observeEvent(
  input$saveconfig,{
    if (nchar(input$configsavename)>0){
      
      
      if(!is.null(input$tempgrossmaxfail)){
        if (nchar(input$tempgrossmaxfail)>0){
          tempgrossmaxfail = input$tempgrossmaxfail
        }else{
          tempgrossmaxfail = ContData.env$myThresh.Gross.Fail.Hi.WaterTemp
        }
      }else{
        tempgrossmaxfail = ContData.env$myThresh.Gross.Fail.Hi.WaterTemp
      }
      
      if(!is.null(input$tempgrossminfail)){
        if (nchar(input$tempgrossminfail)>0){
          tempgrossminfail = input$tempgrossminfail
        }else{
          tempgrossminfail = ContData.env$myThresh.Gross.Fail.Lo.WaterTemp
        }
      }else{
        tempgrossminfail = ContData.env$myThresh.Gross.Fail.Lo.WaterTemp
      }
      
      if(!is.null(input$tempgrossmaxsusp)){
        if (nchar(input$tempgrossmaxsusp)>0){
          tempgrossmaxsusp = input$tempgrossmaxsusp
        }else{
          tempgrossmaxsusp = ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp
        }
      }else{
        tempgrossmaxsusp = ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp
      }
      
      if(!is.null(input$tempgrossminsusp)){
        if (nchar(input$tempgrossminsusp)>0){
          tempgrossminsusp = input$tempgrossminsusp
        }else{
          tempgrossminsusp = ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp
        }
      }else{
        tempgrossminsusp = ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp
      }
      
      if(!is.null(input$tempspikemaxfail)){
        if (nchar(input$tempspikemaxfail)>0){
          tempspikemaxfail = input$tempspikemaxfail
        }else{
          tempspikemaxfail = ContData.env$myThresh.Spike.Hi.WaterTemp
        }
      }else{tempspikemaxfail = ContData.env$myThresh.Spike.Hi.WaterTemp}
      
      if(!is.null(input$tempspikeminfail)){
        if (nchar(input$tempspikeminfail)>0){
          tempspikeminfail = input$tempspikeminfail
        }else{
          tempspikeminfail = ContData.env$myThresh.Spike.Lo.WaterTemp
        }
      }else{
        tempspikeminfail = ContData.env$myThresh.Spike.Lo.WaterTemp
      }

      if(!is.null(input$temprocsd)){
        if (nchar(input$temprocsd)>0){
          temprocsd = input$temprocsd
        }else{
          temprocsd = ContData.env$myThresh.RoC.SD.number.WaterTemp
        }
      }else{
        temprocsd = ContData.env$myThresh.RoC.SD.number.WaterTemp
      }
      
      if(!is.null(input$temprocperiod)){
        if (nchar(input$temprocperiod)>0){
          temprocperiod = input$temprocperiod
        }else{
          temprocperiod = ContData.env$myThresh.RoC.SD.period.WaterTemp
        }
      }else{
        temprocperiod = ContData.env$myThresh.RoC.SD.period.WaterTemp
      }
      
      if(!is.null(input$tempflathi)){
        if (nchar(input$tempflathi)>0){
          tempflathi = input$tempflathi
        }else{
          tempflathi = ContData.env$myThresh.Flat.Hi.WaterTemp
        }
      }else{
        tempflathi = ContData.env$myThresh.Flat.Hi.WaterTemp
      }
      
      if(!is.null(input$tempflatlow)){
        if (nchar(input$tempflatlow)>0){
          tempflatlow = input$tempflatlo
        }else{
          tempflatlow = ContData.env$myThresh.Flat.Lo.WaterTemp
        }
      }else{
        tempflatlow = ContData.env$myThresh.Flat.Lo.WaterTemp
      }
      
      if(!is.null(input$tempflattol)){
        if (nchar(input$tempflattol)>0){
          tempflattol = input$tempflattol
        }else{
          tempflattol = ContData.env$myThresh.Flat.Tolerance.WaterTemp
        }
      }else{
        tempflattol = ContData.env$myThresh.Flat.Tolerance.WaterTemp
      }
      
      if(!is.null(input$dogrossmaxfail)){
        if (nchar(input$dogrossmaxfail)>0){
          dogrossmaxfail = input$dogrossmaxfail
        }else{
          dogrossmaxfail = ContData.env$myThresh.Gross.Fail.Hi.DO
        }
      }else{
        dogrossmaxfail = ContData.env$myThresh.Gross.Fail.Hi.DO
      }
      
      if(!is.null(input$dogrossminfail)){
        if (nchar(input$dogrossminfail)>0){
          dogrossminfail = input$dogrossminfail
        }else{
          dogrossminfail = ContData.env$myThresh.Gross.Fail.Lo.DO
        }
      }else{
        dogrossminfail = ContData.env$myThresh.Gross.Fail.Lo.DO
      }
      
      if(!is.null(input$dogrossmaxsusp)){
        if (nchar(input$dogrossmaxsusp)>0){
          dogrossmaxsusp = input$dogrossmaxsusp
        }else{
          dogrossmaxsusp = ContData.env$myThresh.Gross.Suspect.Hi.DO
        }
      }else{
        dogrossmaxsusp = ContData.env$myThresh.Gross.Suspect.Hi.DO
      }
      
      if(!is.null(input$dogrossminsusp)){
        if (nchar(input$dogrossminsusp)>0){
          dogrossminsusp = input$dogrossminsusp
        }else{
          dogrossminsusp = ContData.env$myThresh.Gross.Suspect.Lo.DO
        }
      }else{
        dogrossminsusp = ContData.env$myThresh.Gross.Suspect.Lo.DO
      }
      
      if(!is.null(input$dospikemaxfail)){
        if (nchar(input$dospikemaxfail)>0){
          dospikemaxfail = input$dospikemaxfail
        }else{
          dospikemaxfail = ContData.env$myThresh.Spike.Hi.DO
        }
      }else{
        dospikemaxfail = ContData.env$myThresh.Spike.Hi.DO
      }
      
      if(!is.null(input$dospikeminfail)){
      if (nchar(input$dospikeminfail)>0){
        dospikeminfail = input$dospikeminfail
      }else{
        dospikeminfail = ContData.env$myThresh.Spike.Lo.DO
      }
      }else{
        dospikeminfail = ContData.env$myThresh.Spike.Lo.DO
      }
      
      if(!is.null(input$dorocsd)){
        if (nchar(input$dorocsd)>0){
          dorocsd = input$dorocsd
        }else{
          dorocsd = ContData.env$myThresh.RoC.SD.number.DO
        }
      }else{
        dorocsd = ContData.env$myThresh.RoC.SD.number.DO
      }
      
      if(!is.null(input$dorocperiod)){
        if (nchar(input$dorocperiod)>0){
          dorocperiod = input$dorocperiod
        }else{
          dorocperiod = ContData.env$myThresh.RoC.SD.period.DO
        }
      }else{
        dorocperiod = ContData.env$myThresh.RoC.SD.period.DO
      }
      
      if(!is.null(input$doflathi)){
        if (nchar(input$doflathi)>0){
          doflathi = input$doflathi
        }else{
          doflathi = ContData.env$myThresh.Flat.Hi.DO
        }
      }else{
        doflathi = ContData.env$myThresh.Flat.Hi.DO
      }
      
      if(!is.null(input$doflatlow)){
        if (nchar(input$doflatlow)>0){
          doflatlow = input$doflatlow
        }else{
          doflatlow = ContData.env$myThresh.Flat.Lo.DO
        }
      }else{
        doflatlow = ContData.env$myThresh.Flat.Lo.DO
      }
      
      if(!is.null(input$doflattol)){
        if (nchar(input$doflattol)>0){
          doflattol = input$doflattol
        }else{
          doflattol = ContData.env$myThresh.Flat.Tolerance.DO
        }
      }else{
        doflattol = ContData.env$myThresh.Flat.Tolerance.DO 
      }
      
      configsettingsoutput = paste0(
                                    "ContData.env$myThresh.Gross.Fail.Hi.WaterTemp = ",tempgrossmaxfail,"\n",
                                    "ContData.env$myThresh.Gross.Fail.Lo.WaterTemp = ",tempgrossminfail,"\n",
                                    "ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp = ",tempgrossmaxsusp,"\n",
                                    "ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp = ",tempgrossminsusp,"\n",
                                    "ContData.env$myThresh.Spike.Hi.WaterTemp = ",tempspikemaxfail,"\n",
                                    "ContData.env$myThresh.Spike.Lo.WaterTemp = ",tempspikeminfail,"\n",
                                    "ContData.env$myThresh.RoC.SD.number.WaterTemp = ",temprocsd,"\n",
                                    "ContData.env$myThresh.RoC.SD.period.WaterTemp = ",temprocperiod,"\n",
                                    "ContData.env$myThresh.Flat.Hi.WaterTemp = ",tempflathi,"\n",
                                    "ContData.env$myThresh.Flat.Lo.WaterTemp = ",tempflatlow,"\n",
                                    "ContData.env$myThresh.Flat.Tolerance.WaterTemp = ",tempflattol,"\n",
                                    "ContData.env$myThresh.Gross.Fail.Hi.DO = ",dogrossmaxfail,"\n",
                                    "ContData.env$myThresh.Gross.Fail.Lo.DO = ",dogrossminfail,"\n",
                                    "ContData.env$myThresh.Gross.Suspect.Hi.DO = ",dogrossmaxsusp,"\n",
                                    "ContData.env$myThresh.Gross.Suspect.Lo.DO = ",dogrossminsusp,"\n",
                                    "ContData.env$myThresh.Spike.Hi.DO = ",dospikemaxfail,"\n",
                                    "ContData.env$myThresh.Spike.Lo.DO = ",dospikeminfail,"\n",
                                    "ContData.env$myThresh.RoC.SD.number.DO = ",dorocsd,"\n",
                                    "ContData.env$myThresh.RoC.SD.period.DO = ",dorocperiod,"\n",
                                    "ContData.env$myThresh.Flat.Hi.DO = ",doflathi,"\n",
                                    "ContData.env$myThresh.Flat.Lo.DO = ",doflatlow,"\n",
                                    "ContData.env$myThresh.Flat.Tolerance.DO = ",doflattol,"\n")
      
      cat(configsettingsoutput,file = paste0("config/",input$configsavename,".R"))
      
      configfiles=gsub(".R","",list.files("config"))
      configfiles=configfiles[configfiles != "default_orig"]
      
      updatePickerInput(
        session = session,
        inputId = "configfile",
        choices = configfiles,
        selected = input$configsavename
      )

    }else{
      sendSweetAlert(
        session,
        title = "Missing Save Name",
        text = "Add name for the Save Configuration File",
        type = "warning"
      )
    }
  }
)