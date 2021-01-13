#Build the config file used by ContDataQC
observe({
  qcconfigdata=qc_config()
  qcconfigdata=qcconfigdata[which(qcconfigdata$AppID == input$procwaterbody),]
  
  
  grossfailhi=qcconfigdata[which(qcconfigdata$QC_Metric == "Gross.Fail.Hi"),]
  grossfaillo=qcconfigdata[which(qcconfigdata$QC_Metric == "Gross.Fail.Lo"),]
  grosssuspecthi=qcconfigdata[which(qcconfigdata$QC_Metric == "Gross.Suspect.Hi"),]
  grosssuspectlo=qcconfigdata[which(qcconfigdata$QC_Metric == "Gross.Suspect.Lo"),]
  spikehi=qcconfigdata[which(qcconfigdata$QC_Metric == "Spike.Hi"),]
  spikelo=qcconfigdata[which(qcconfigdata$QC_Metric == "Spike.Lo"),]
  rocsdnumber=qcconfigdata[which(qcconfigdata$QC_Metric == "RoC.SD.number"),]
  rocsdperiod=qcconfigdata[which(qcconfigdata$QC_Metric == "RoC.SD.period"),]
  flathi=qcconfigdata[which(qcconfigdata$QC_Metric == "Flat.Hi"),]
  flatlo=qcconfigdata[which(qcconfigdata$QC_Metric == "Flat.Lo"),]
  flattolerance=qcconfigdata[which(qcconfigdata$QC_Metric == "Flat.Tolerance"),]
  
  rowidname = "RowID"
  
  configsettingsoutput = paste0(
    "print(\"Start Custom Config\")",
    
    #WaterBody
    "# Waterbody ID:",input$procwaterbody,"\n\n",
    
    "#--------------------------------------------------------------------\n",
    "# Continuous data helper script\n
    # Default Values\n
    # Erik.Leppo@tetratech.com (EWL)\n
    # 20150928\n
    # 20170323, add 3 parameters (Cond, DO, pH)\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    # User defined variable names for input data\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    # It is assumed that this R script is stored in a directory with the data files as subdirectories\n
    # This script is intended to be \"source\"d from the main script.\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    # @keywords continuous data\n
    # @examples\n
    # #Not intended to be accessed indepedant of function ContDataQC().\n
    # #Data values only.  No functions.  Add to environment so only visible inside library.\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    # USER may make modifications in this section but not mandatory\n
    # this section could be sourced so can use between scripts\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    #UserDefinedValues <- NA # default value so shows up in help files\n
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
    # assign variables to new environment requires editing of all lines.\n
    # For example, myDelim <- \"_\" BECOMES ContData.env$myDelim, \"_\"\n
    ###\n
    # list all elements in environment\n
    # ls(ContData.env)  # all elements in environment\n
    # as.list(ContData.env)  # all elements in environment with assigned values\n\n",

    
    "#--------------------------------------------------------------------\n",
    "# Delimiter in File Names (e.g., test2_AW_201301_20131231.csv)\n",
    "ContData.env$myDelim <- \"_\"\n",
    "ContData.env$myDelim_LakeID <- \"~\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Basic
    "# Basic\n",
    "ContData.env$myName.SiteID <- \"SiteID\"\n",
    "ContData.env$myName.Date <- \"Date\"\n",
    "ContData.env$myName.Time <- \"Time\"\n",
    "ContData.env$myName.DateTime <- \"DateTime\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #QC Units
    "#QC Units\n",
    "ContData.env$myUnits.AirBP <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirBP")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.AirTemp <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirTemp")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.Chlorophylla <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Chlorophylla")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.Cond <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Cond")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.Discharge <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Discharge")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.DO <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "DO")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.GageHeight <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "GageHeight")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.pH <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "pH")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.Turbidity <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Turbidity")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.WaterP <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterP")]),perl = TRUE)
      ),"\"\n",
    
    "ContData.env$myUnits.WaterTemp <- \"",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterTemp")]),perl = TRUE)
      ),"\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Logger Fields
    "# Logger Fields\n",
    "ContData.env$myName.RowID.Water <- \"",rowidname,"\"\n",
    "ContData.env$myName.LoggerID.Water <- \"",rowidname,"\"\n",
    "ContData.env$myName.RowID.Air <- \"", rowidname,"\"\n",
    "ContData.env$myName.LoggerID.Air <- \"", rowidname,"\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Parameters as appear in logger files
    "# Parameters as appear in logger files\n",
    "ContData.env$myName.AirBP <- \"AirBP",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirBP")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.AirTemp <- \"AirTemp",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirTemp")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.Chlorophylla <- \"Chlorophylla",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Chlorophylla")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.Cond <- \"Cond",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Cond")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.Discharge <- \"Discharge",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Discharge")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.DO <- \"DO",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "DO")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.GageHeight <- \"GageHeight",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "GageHeight")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.pH <- \"pH",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "pH")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.Turbidity <- \"Turbidity",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "Turbidity")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.WaterP <- \"WaterP",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterP")]),perl = TRUE)
    ),"\"\n",
    "ContData.env$myName.WaterTemp <- \"WaterTemp",gsub(
      "Â","",gsub("[^[:alnum:][:blank:]?&/\\-]", "",make.names(grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterTemp")]),perl = TRUE)
    ),"\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Plot Labels
    "#Plot Labels\n",
    "ContData.env$myLab.Date <- \"Date\"\n",
    "ContData.env$myLab.DateTime <- \"Date\"\n",
    "ContData.env$myLab.AirBP <- \"",paste0("Barometric Pressure, Air (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirBP")],")"),"\"\n",
    "ContData.env$myLab.AirTemp <- \"",paste0("Temperature, Air (deg ",grossfailhi$Unit[which(grossfailhi$Logger_Type == "AirTemp")],")"),"\"\n",
    "ContData.env$myLab.Chlorophylla <- \"",paste0("Chlorophyll a (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "Chlorophylla")],")"),"\"\n",
    "ContData.env$myLab.Cond <- \"",paste0("Conductivity (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "Cond")],")"),"\"\n",
    "ContData.env$myLab.Discharge <- \"",paste0("Discharge (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "Discharge")],")"),"\"\n",
    "ContData.env$myLab.DO <- \"",paste0("Dissolved Oxygen (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "DO")],")"),"\"\n",
    "ContData.env$myLab.GageHeight <- \"",paste0("Gage Height (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "GageHeight")],")"),"\"\n",
    "ContData.env$myLab.pH <- \"",paste0("pH (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "pH")],")"),"\"\n",
    "ContData.env$myLab.Turbidity <- \"",paste0("Turbidity (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "Turbidity")],")"),"\"\n",
    "ContData.env$myLab.WaterP <- \"",paste0("Pressure, Water (",grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterP")],")"),"\"\n",
    "ContData.env$myLab.WaterTemp <- \"",paste0("Temperature, Water (deg ",grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterTemp")],")"),"\"\n",
    "ContData.env$myLab.Temp.BOTH <- \"",paste0("Temperature (deg ",grossfailhi$Unit[which(grossfailhi$Logger_Type == "WaterTemp")],")"),"\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Discrete Measurements
    "#Discrete Measurements\n",
    "ContData.env$myPrefix.Discrete <- \"Discrete\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Discrete Names
    "#Discrete Names\n",
    "ContData.env$myName.Discrete.AirBP <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.AirBP)\n",
    "ContData.env$myName.Discrete.AirTemp <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.AirTemp)\n",
    "ContData.env$myName.Discrete.Chlorophylla <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Chlorophylla)\n",
    "ContData.env$myName.Discrete.Cond <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Cond)\n",
    "ContData.env$myName.Discrete.Discharge <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Discharge)\n",
    "ContData.env$myName.Discrete.DO <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.DO)\n",
    "ContData.env$myName.Discrete.GageHeight <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.GageHeight)\n",
    "ContData.env$myName.Discrete.pH <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.pH)\n",
    "ContData.env$myName.Discrete.Turbidity <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Turbidity)\n",
    "ContData.env$myName.Discrete.WaterP <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.WaterP)\n",
    "ContData.env$myName.Discrete.WaterTemp <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.WaterTemp)\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Discrete Labels
    "#Discrete Labels\n",
    "ContData.env$myLab.Discrete.AirBP <- paste(ContData.env$myLab.AirBP,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.AirTemp <- paste(ContData.env$myLab.AirTemp,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.Chlorophylla <- paste(ContData.env$myLab.Chlorophylla,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.Cond <- paste(ContData.env$myLab.Cond,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.Discharge <- paste(ContData.env$myLab.Discharge,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.DO <- paste(ContData.env$myLab.DO,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.GageHeight <- paste(ContData.env$myLab.GageHeight,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.pH <- paste(ContData.env$myLab.pH,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.Turbidity <- paste(ContData.env$myLab.Turbidity,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.WaterP <- paste(ContData.env$myLab.WaterP,\"(Discrete)\")\n",
    "ContData.env$myLab.Discrete.WaterTemp <- paste(ContData.env$myLab.WaterTemp,\"(Discrete)\")\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Automated QC
    "# Automated QC stuff ####\n",
    "## data type/stages\n",
    "ContData.env$myDataQuality.Raw <- \"RAW\"\n",
    "ContData.env$myDataQuality.QCauto <- \"QCauto\"\n",
    "ContData.env$myDataQuality.QCmanual <- \"QCmanual\"\n",
    "ContData.env$myDataQuality.Final <- \"Final\"\n",
    "ContData.env$myDataQuality.Aggregated <- \"Aggregated\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Directory Names
    "# Directory Names ####\n",
    "ContData.env$myName.Dir.0Original <- \"Data0_Original\"\n",
    "ContData.env$myName.Dir.1Raw <- \"Data1_Raw\"\n",
    "ContData.env$myName.Dir.2QC <- \"Data2_QC\"\n",
    "ContData.env$myName.Dir.3Agg <- \"Data3_Aggregated\"\n",
    "ContData.env$myName.Dir.4Stats <- \"Data4_Stats\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Data Fields
    "# Data Fields\n",
    "ContData.env$myNames.DataFields <- c(
      ContData.env$myName.WaterTemp, 
      ContData.env$myName.AirTemp, 
      ContData.env$myName.WaterP, 
      ContData.env$myName.AirBP, 
      ContData.env$myName.SensorDepth, 
      ContData.env$myName.Discharge, 
      ContData.env$myName.Cond, 
      ContData.env$myName.DO, 
      ContData.env$myName.pH, 
      ContData.env$myName.Turbidity, 
      ContData.env$myName.Chlorophylla, 
      ContData.env$myName.GageHeight, 
      ContData.env$myName.Discrete.WaterTemp, 
      ContData.env$myName.Discrete.AirTemp, 
      ContData.env$myName.Discrete.WaterP, 
      ContData.env$myName.Discrete.AirBP, 
      ContData.env$myName.Discrete.SensorDepth, 
      ContData.env$myName.Discrete.Discharge, 
      ContData.env$myName.Discrete.Cond, 
      ContData.env$myName.Discrete.DO, 
      ContData.env$myName.Discrete.pH, 
      ContData.env$myName.Discrete.Turbidity, 
      ContData.env$myName.Discrete.Chlorophylla, 
      ContData.env$myName.Discrete.GageHeight
    )\n\n",
    
    "ContData.env$myNames.DataFields.Lab <- c(
      ContData.env$myLab.WaterTemp, 
      ContData.env$myLab.AirTemp, 
      ContData.env$myLab.WaterP, 
      ContData.env$myLab.AirBP, 
      ContData.env$myLab.SensorDepth, 
      ContData.env$myLab.Discharge, 
      ContData.env$myLab.Cond, 
      ContData.env$myLab.DO, 
      ContData.env$myLab.pH, 
      ContData.env$myLab.Turbidity, 
      ContData.env$myLab.Chlorophylla, 
      ContData.env$myLab.GageHeight, 
      ContData.env$myLab.Discrete.WaterTemp, 
      ContData.env$myLab.Discrete.AirTemp, 
      ContData.env$myLab.Discrete.WaterP, 
      ContData.env$myLab.Discrete.AirBP, 
      ContData.env$myLab.Discrete.SensorDepth, 
      ContData.env$myLab.Discrete.Discharge, 
      ContData.env$myLab.Discrete.Cond, 
      ContData.env$myLab.Discrete.DO, 
      ContData.env$myLab.Discrete.pH, 
      ContData.env$myLab.Discrete.Turbidity, 
      ContData.env$myLab.Discrete.Chlorophylla, 
      ContData.env$myLab.Discrete.GageHeight
    )\n\n",
    
    "ContData.env$myNames.DataFields.Col <- c(
      \"blue\",\"green\",\"gray\",\"gray\",\"black\",\"brown\",\"purple\",\"orange\",\"salmon\",\"rosybrown\",\"aquamarine1\"
    )\n\n",
    
    # Name Order (change order below to change order in output file)
    "# Name Order (change order below to change order in output file)\n",
    "ContData.env$myNames.Order <- c(
      ContData.env$myName.SiteID, 
      ContData.env$myName.Date, 
      ContData.env$myName.Time, 
      ContData.env$myName.DateTime, 
      ContData.env$myName.WaterTemp, 
      ContData.env$myName.LoggerID.Air, 
      ContData.env$myName.RowID.Air, 
      ContData.env$myName.AirTemp, 
      ContData.env$myName.WaterP, 
      ContData.env$myName.AirBP, 
      ContData.env$myName.SensorDepth, 
      ContData.env$myName.Discharge, 
      ContData.env$myName.Cond, 
      ContData.env$myName.DO, 
      ContData.env$myName.pH, 
      ContData.env$myName.Turbidity, 
      ContData.env$myName.Chlorophylla, 
      ContData.env$myName.GageHeight, 
      ContData.env$myName.LoggerID.Water, 
      ContData.env$myName.RowID.Water, 
      ContData.env$myName.Discrete.WaterTemp, 
      ContData.env$myName.Discrete.AirTemp, 
      ContData.env$myName.Discrete.WaterP, 
      ContData.env$myName.Discrete.AirBP, 
      ContData.env$myName.Discrete.SensorDepth, 
      ContData.env$myName.Discrete.Discharge, 
      ContData.env$myName.Discrete.Cond, 
      ContData.env$myName.Discrete.DO, 
      ContData.env$myName.Discrete.pH, 
      ContData.env$myName.Discrete.Turbidity, 
      ContData.env$myName.Discrete.Chlorophylla, 
      ContData.env$myName.Discrete.GageHeight
    )\n\n",
    
    "#--------------------------------------------------------------------\n",
    ## Data Quality Flag Values
    "## Data Quality Flag Values/n",
    "ContData.env$myFlagVal.Pass <- \"P\"\n",
    "ContData.env$myFlagVal.NotEval <- \"NA\"\n",
    "ContData.env$myFlagVal.Suspect <- \"S\"\n",
    "ContData.env$myFlagVal.Fail <- \"F\"\n",
    "ContData.env$myFlagVal.NoData <- \"X\"\n",
    "ContData.env$myFlagVal.Order <- c(
      ContData.env$myFlagVal.Pass, 
      ContData.env$myFlagVal.Suspect, 
      ContData.env$myFlagVal.Fail, 
      ContData.env$myFlagVal.NoData
    )\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Gross Fail Hi
    "#Gross Fail Hi\n",
    "ContData.env$myThresh.Gross.Fail.Hi.AirBP <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.AirTemp <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.Chlorophylla <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.Cond <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.Discharge <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.DO <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.GageHeight <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.pH <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.Turbidity <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.WaterP <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Gross.Fail.Hi.WaterTemp <- ",grossfailhi$Value[which(grossfailhi$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Gross Fail Lo
    "#Gross Fail Lo\n",
    "ContData.env$myThresh.Gross.Fail.Lo.AirBP <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.AirTemp <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.Chlorophylla <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.Cond <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.Discharge <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.DO <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.GageHeight <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.pH <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.Turbidity <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.WaterP <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Gross.Fail.Lo.WaterTemp <- ",grossfaillo$Value[which(grossfaillo$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Gross Suspect Hi
    "#Gross Suspect Hi\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.AirBP <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.AirTemp <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.Chlorophylla <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.Cond <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.Discharge <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.DO <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.GageHeight <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.pH <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.Turbidity <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.WaterP <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp <- ",grosssuspecthi$Value[which(grosssuspecthi$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Gross Suspect Lo
    "#Gross Suspect Lo\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.AirBP <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.AirTemp <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.Chlorophylla <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.Cond <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.Discharge <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.DO <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.GageHeight <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.pH <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.Turbidity <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.WaterP <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp <- ",grosssuspectlo$Value[which(grosssuspectlo$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Spike Hi
    "#Spike Hi\n",
    "ContData.env$myThresh.Spike.Hi.AirBP <- ",spikehi$Value[which(spikehi$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Spike.Hi.AirTemp <- ",spikehi$Value[which(spikehi$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Spike.Hi.Chlorophylla <- ",spikehi$Value[which(spikehi$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Spike.Hi.Cond <- ",spikehi$Value[which(spikehi$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Spike.Hi.Discharge <- ",spikehi$Value[which(spikehi$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Spike.Hi.DO <- ",spikehi$Value[which(spikehi$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Spike.Hi.GageHeight <- ",spikehi$Value[which(spikehi$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Spike.Hi.pH <- ",spikehi$Value[which(spikehi$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Spike.Hi.Turbidity <- ",spikehi$Value[which(spikehi$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Spike.Hi.WaterP <- ",spikehi$Value[which(spikehi$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Spike.Hi.WaterTemp <- ",spikehi$Value[which(spikehi$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Spike Lo
    "#Spike Lo\n",
    "ContData.env$myThresh.Spike.Lo.AirBP <- ",spikelo$Value[which(spikelo$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Spike.Lo.AirTemp <- ",spikelo$Value[which(spikelo$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Spike.Lo.Chlorophylla <- ",spikelo$Value[which(spikelo$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Spike.Lo.Cond <- ",spikelo$Value[which(spikelo$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Spike.Lo.Discharge <- ",spikelo$Value[which(spikelo$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Spike.Lo.DO <- ",spikelo$Value[which(spikelo$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Spike.Lo.GageHeight <- ",spikelo$Value[which(spikelo$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Spike.Lo.pH <- ",spikelo$Value[which(spikelo$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Spike.Lo.Turbidity <- ",spikelo$Value[which(spikelo$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Spike.Lo.WaterP <- ",spikelo$Value[which(spikelo$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Spike.Lo.WaterTemp <- ",spikelo$Value[which(spikelo$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #RoC SD number
    "#RoC SD number\n",
    "ContData.env$myThresh.RoC.SD.number.AirBP <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.RoC.SD.number.AirTemp <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.RoC.SD.number.Chlorophylla <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.RoC.SD.number.Cond <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.RoC.SD.number.Discharge <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.RoC.SD.number.DO <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.RoC.SD.number.GageHeight <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.RoC.SD.number.pH <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.RoC.SD.number.Turbidity <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.RoC.SD.number.WaterP <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.RoC.SD.number.WaterTemp <- ",rocsdnumber$Value[which(rocsdnumber$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #RoC SD period
    "#RoC SD period\n",
    "ContData.env$myThresh.RoC.SD.period.AirBP <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.RoC.SD.period.AirTemp <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.RoC.SD.period.Chlorophylla <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.RoC.SD.period.Cond <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.RoC.SD.period.Discharge <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.RoC.SD.period.DO <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.RoC.SD.period.GageHeight <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.RoC.SD.period.pH <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.RoC.SD.period.Turbidity <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.RoC.SD.period.WaterP <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.RoC.SD.period.WaterTemp <- ",rocsdperiod$Value[which(rocsdperiod$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Flat Hi
    "#Flat Hi\n",
    "ContData.env$myThresh.Flat.Hi.AirBP <- ",flathi$Value[which(flathi$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Flat.Hi.AirTemp <- ",flathi$Value[which(flathi$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Flat.Hi.Chlorophylla <- ",flathi$Value[which(flathi$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Flat.Hi.Cond <- ",flathi$Value[which(flathi$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Flat.Hi.Discharge <- ",flathi$Value[which(flathi$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Flat.Hi.DO <- ",flathi$Value[which(flathi$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Flat.Hi.GageHeight <- ",flathi$Value[which(flathi$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Flat.Hi.pH <- ",flathi$Value[which(flathi$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Flat.Hi.Turbidity <- ",flathi$Value[which(flathi$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Flat.Hi.WaterP <- ",flathi$Value[which(flathi$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Flat.Hi.WaterTemp <- ",flathi$Value[which(flathi$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Flat Lo
    "#Flat Lo\n",
    "ContData.env$myThresh.Flat.Lo.AirBP <- ",flatlo$Value[which(flathi$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Flat.Lo.AirTemp <- ",flatlo$Value[which(flathi$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Flat.Lo.Chlorophylla <- ",flatlo$Value[which(flathi$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Flat.Lo.Cond <- ",flatlo$Value[which(flathi$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Flat.Lo.Discharge <- ",flatlo$Value[which(flathi$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Flat.Lo.DO <- ",flatlo$Value[which(flathi$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Flat.Lo.GageHeight <- ",flatlo$Value[which(flathi$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Flat.Lo.pH <- ",flatlo$Value[which(flathi$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Flat.Lo.Turbidity <- ",flatlo$Value[which(flathi$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Flat.Lo.WaterP <- ",flatlo$Value[which(flathi$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Flat.Lo.WaterTemp <- ",flatlo$Value[which(flathi$Logger_Type == "WaterTemp")],"\n\n",
    
    "#--------------------------------------------------------------------\n",
    #Flat Tolerance
    "#Flat Tolerance\n",
    "ContData.env$myThresh.Flat.Tolerance.AirBP <- ",flattolerance$Value[which(flattolerance$Logger_Type == "AirBP")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.AirTemp <- ",flattolerance$Value[which(flattolerance$Logger_Type == "AirTemp")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.Chlorophylla <- ",flattolerance$Value[which(flattolerance$Logger_Type == "Chlorophylla")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.Cond <- ",flattolerance$Value[which(flattolerance$Logger_Type == "Cond")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.Discharge <- ",flattolerance$Value[which(flattolerance$Logger_Type == "Discharge")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.DO <- ",flattolerance$Value[which(flattolerance$Logger_Type == "DO")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.GageHeight <- ",flattolerance$Value[which(flattolerance$Logger_Type == "GageHeight")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.pH <- ",flattolerance$Value[which(flattolerance$Logger_Type == "pH")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.Turbidity <- ",flattolerance$Value[which(flattolerance$Logger_Type == "Turbidity")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.WaterP <- ",flattolerance$Value[which(flattolerance$Logger_Type == "WaterP")],"\n",
    "ContData.env$myThresh.Flat.Tolerance.WaterTemp <- ",flattolerance$Value[which(flattolerance$Logger_Type == "WaterTemp")],"\n\n",
    
    "ContData.env$myThresh.Flat.MaxComp    <- max(
      ContData.env$myThresh.Flat.Hi.WaterTemp, 
      ContData.env$myThresh.Flat.Hi.AirTemp, 
      ContData.env$myThresh.Flat.Hi.WaterP, 
      ContData.env$myThresh.Flat.Hi.AirBP, 
      ContData.env$myThresh.Flat.Hi.Discharge, 
      ContData.env$myThresh.Flat.Hi.Cond, 
      ContData.env$myThresh.Flat.Hi.DO, 
      ContData.env$myThresh.Flat.Hi.pH, 
      ContData.env$myThresh.Flat.Hi.Turbidity, 
      ContData.env$myThresh.Flat.Hi.Chlorophylla, 
      ContData.env$myThresh.Flat.Hi.GageHeight
    )\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Data Fields with Flags
    "# Data Fields with Flags\n",
    "ContData.env$myName.Flag <- \"Flag\"\n",
    "ContData.env$myNames.Cols4Flags <- c(ContData.env$myName.DateTime,ContData.env$myNames.DataFields)\n",
    "ContData.env$myNames.Flags <- paste(ContData.env$myName.Flag,ContData.env$myNames.Cols4Flags)\n",
    "ContData.env$myName.Flag.DateTime <- paste(ContData.env$myName.Flag,ContData.env$myName.DateTime)\n",
    "ContData.env$myName.Flag.AirBP <- paste(ContData.env$myName.Flag,ContData.env$myName.AirBP)\n",
    "ContData.env$myName.Flag.AirTemp <- paste(ContData.env$myName.Flag,ContData.env$myName.AirTemp)\n",
    "ContData.env$myName.Flag.Chlorophylla <- paste(ContData.env$myName.Flag,ContData.env$myName.Chlorophylla)\n",
    "ContData.env$myName.Flag.Cond <- paste(ContData.env$myName.Flag,ContData.env$myName.Cond)\n",
    "ContData.env$myName.Flag.Discharge <- paste(ContData.env$myName.Flag,ContData.env$myName.Discharge)\n",
    "ContData.env$myName.Flag.DO <- paste(ContData.env$myName.Flag,ContData.env$myName.DO)\n",
    "ContData.env$myName.Flag.GageHeight <- paste(ContData.env$myName.Flag,ContData.env$myName.GageHeight)\n",
    "ContData.env$myName.Flag.pH <- paste(ContData.env$myName.Flag,ContData.env$myName.pH)\n",
    "ContData.env$myName.Flag.Turbidity <- paste(ContData.env$myName.Flag,ContData.env$myName.Turbidity)\n",
    "ContData.env$myName.Flag.WaterP <- paste(ContData.env$myName.Flag,ContData.env$myName.WaterP)\n",
    "ContData.env$myName.Flag.WaterTemp <- paste(ContData.env$myName.Flag,ContData.env$myName.WaterTemp)\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Data Quality Test Names
    "# Data Quality Test Names/n",
    "ContData.env$myNames.QCTests <- c(\"Gross\",\"Spike\",\"RoC\",\"Flat\")\n",
    "ContData.env$myNames.QCCalcs <- c(\"SD.Time\",\"SD\",\"SDxN\",paste(\"n\",1:ContData.env$myThresh.Flat.MaxComp),\"flat.Lo\",\"flat.Hi\"\n\n)",
    
    "#--------------------------------------------------------------------\n",
    # Exceedance values for stats (default to Gross-Suspect-Hi value)
    "# Exceedance values for stats (default to Gross-Suspect-Hi value)/n",
    "ContData.env$myExceed.WaterTemp  <- ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp/n",
    "ContData.env$myExceed.AirTemp    <- ContData.env$myThresh.Gross.Suspect.Hi.AirTemp/n",
    "ContData.env$myExceed.SensorDepth <- ContData.env$myThresh.Gross.Suspect.Hi.SensorDepth/n/n",
    
    "#--------------------------------------------------------------------\n",
    # Date and Time Formats
    "# Date and Time Formats\n",
    "ContData.env$myFormat.Date <- \"%Y-%m-%d\"\n",
    "ContData.env$myFormat.Time <- \"%H:%M:%S\"\n",
    "ContData.env$myFormat.DateTime <- \"%Y-%m-%d %H:%M:%S\"\n",
    "ContData.env$DateRange.Start.Default <- format(as.Date(\"1900-01-01\"),ContData.env$myFormat.Date)\n",
    "ContData.env$DateRange.End.Default <- format(Sys.Date(),ContData.env$myFormat.Date)\n",
    "# Time Zone, used in Gage script in dataRetrieval, OlsonNames()/n",
    "ContData.env$myTZ <- Sys.timezone()/n/n",
    
    "#--------------------------------------------------------------------\n",
    # Time Frames (MM-DD)
    "# Time Frames (MM-DD)/n",
    "ContData.env$myTimeFrame.Annual.Start <- \"0101\"\n",
    "ContData.env$myTimeFrame.Annual.End <- \"1231\"\n",
    "ContData.env$myTimeFrame.WaterYear.Start <- \"1001\"\n",
    "#ContData.env$myTimeFrame.WaterYear.End <- \"0930\"\n",
    "ContData.env$myTimeFrame.Season.Spring.Start <- \"0301\"\n",
    "#ContData.env$myTimeFrame.Season.Spring.End <- \"0531\"\n",
    "ContData.env$myTimeFrame.Season.Summer.Start <- \"0601\"\n",
    "#ContData.env$myTimeFrame.Season.Summer.End <- \"0831\"\n",
    "ContData.env$myTimeFrame.Season.Fall.Start <- \"0901\"\n",
    "#ContData.env$myTimeFrame.Season.Fall.End <- \"1130\"\n",
    "ContData.env$myTimeFrame.Season.Winter.Start <- \"1201\"\n",
    "#ContData.env$myTimeFrame.Season.Winter.End <- \"0228\" #but 0229 in leap year, use start dates only\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Time Frame Names
    "# Time Frame Names\n",
    "ContData.env$myName.Yr <- \"Year\"\n",
    "ContData.env$myName.YrMo <- \"YearMonth\"\n",
    "ContData.env$myName.Mo <- \"Month\"\n",
    "ContData.env$myName.MoDa <- \"MonthDay\"\n",
    "ContData.env$myName.JuDa <- \"JulianDay\"\n",
    "ContData.env$myName.Day <- \"Day\"\n",
    "ContData.env$myName.Season <- \"Season\"\n",
    "ContData.env$myName.YrSeason <- \"YearSeason\"\n\n",
    
    "#--------------------------------------------------------------------\n",
    # for summary stats
    "# for summary stats\n",
    "ContData.env$myNames.Fields.TimePeriods <- c(
      ContData.env$myName.Yr,
      ContData.env$myName.YrMo,
      ContData.env$myName.Mo,
      ContData.env$myName.MoDa,
      ContData.env$myName.JuDa,
      ContData.env$myName.Season,
      ContData.env$myName.YrSeason
    )\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Exclude Trigger
    "# Exclude Trigger\n",
    "# Trigger for Stats to exclude (TRUE) or include (FALSE) where flag = \"fail\"\n",
    "ContData.env$myStats.Fails.Exclude <- TRUE  #FALSE #TRUE\n\n",
    
    "#--------------------------------------------------------------------\n",
    # Report Format
    "# Report Format\n",
    "ContData.env$myReport.Format <- \"html\"  # \"html\" or \"docx\" # DOCX requires Pandoc.\n",
    "ContData.env$myReport.Dir <- file.path(system.file(package=\"ContDataQC\"), \"rmd\")"
  )
  
  cat(configsettingsoutput,file = paste0("config/configfile.R"))
})
