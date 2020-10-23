print("Start Custom Config")# Waterbody ID:

#--------------------------------------------------------------------
# Continuous data helper script

    # Default Values

    # Erik.Leppo@tetratech.com (EWL)

    # 20150928

    # 20170323, add 3 parameters (Cond, DO, pH)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # User defined variable names for input data

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # It is assumed that this R script is stored in a directory with the data files as subdirectories

    # This script is intended to be "source"d from the main script.

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # @keywords continuous data

    # @examples

    # #Not intended to be accessed indepedant of function ContDataQC().

    # #Data values only.  No functions.  Add to environment so only visible inside library.

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # USER may make modifications in this section but not mandatory

    # this section could be sourced so can use between scripts

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #UserDefinedValues <- NA # default value so shows up in help files

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # assign variables to new environment requires editing of all lines.

    # For example, myDelim <- "_" BECOMES ContData.env$myDelim, "_"

    ###

    # list all elements in environment

    # ls(ContData.env)  # all elements in environment

    # as.list(ContData.env)  # all elements in environment with assigned values

#--------------------------------------------------------------------
# Delimiter in File Names (e.g., test2_AW_201301_20131231.csv)
ContData.env$myDelim <- "_"
ContData.env$myDelim_LakeID <- "~"

#--------------------------------------------------------------------
# Basic
ContData.env$myName.SiteID <- "SiteID"
ContData.env$myName.Date <- "Date"
ContData.env$myName.Time <- "Time"
ContData.env$myName.DateTime <- "DateTime"

#--------------------------------------------------------------------
#QC Units
ContData.env$myUnits.AirBP <- ""
ContData.env$myUnits.AirTemp <- ""
ContData.env$myUnits.Chlorophylla <- ""
ContData.env$myUnits.Cond <- ""
ContData.env$myUnits.Discharge <- ""
ContData.env$myUnits.DO <- ""
ContData.env$myUnits.GageHeight <- ""
ContData.env$myUnits.pH <- ""
ContData.env$myUnits.Turbidity <- ""
ContData.env$myUnits.WaterP <- ""
ContData.env$myUnits.WaterTemp <- ""

#--------------------------------------------------------------------
# Logger Fields
ContData.env$myName.RowID.Water <- "RowID"
ContData.env$myName.LoggerID.Water <- "RowID"
ContData.env$myName.RowID.Air <- "RowID"
ContData.env$myName.LoggerID.Air <- "RowID"

#--------------------------------------------------------------------
# Parameters as appear in logger files
ContData.env$myName.AirBP <- "AirBP"
ContData.env$myName.AirTemp <- "AirTemp"
ContData.env$myName.Chlorophylla <- "Chlorophylla"
ContData.env$myName.Cond <- "Cond"
ContData.env$myName.Discharge <- "Discharge"
ContData.env$myName.DO <- "DO"
ContData.env$myName.GageHeight <- "GageHeight"
ContData.env$myName.pH <- "pH"
ContData.env$myName.Turbidity <- "Turbidity"
ContData.env$myName.WaterP <- "WaterP"
ContData.env$myName.WaterTemp <- "WaterTemp"

#--------------------------------------------------------------------
#Plot Labels
ContData.env$myLab.Date <- "Date"
ContData.env$myLab.DateTime <- "Date"
ContData.env$myLab.AirBP <- "Barometric Pressure, Air ()"
ContData.env$myLab.AirTemp <- "Temperature, Air (deg )"
ContData.env$myLab.Chlorophylla <- "Chlorophyll a ()"
ContData.env$myLab.Cond <- "Conductivity ()"
ContData.env$myLab.Discharge <- "Discharge ()"
ContData.env$myLab.DO <- "Dissolved Oxygen ()"
ContData.env$myLab.GageHeight <- "Gage Height ()"
ContData.env$myLab.pH <- "pH ()"
ContData.env$myLab.Turbidity <- "Turbidity ()"
ContData.env$myLab.WaterP <- "Pressure, Water ()"
ContData.env$myLab.WaterTemp <- "Temperature, Water (deg )"
ContData.env$myLab.Temp.BOTH <- "Temperature (deg )"

#--------------------------------------------------------------------
#Discrete Measurements
ContData.env$myPrefix.Discrete <- "Discrete"

#--------------------------------------------------------------------
#Discrete Names
ContData.env$myName.Discrete.AirBP <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.AirBP)
ContData.env$myName.Discrete.AirTemp <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.AirTemp)
ContData.env$myName.Discrete.Chlorophylla <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Chlorophylla)
ContData.env$myName.Discrete.Cond <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Cond)
ContData.env$myName.Discrete.Discharge <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Discharge)
ContData.env$myName.Discrete.DO <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.DO)
ContData.env$myName.Discrete.GageHeight <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.GageHeight)
ContData.env$myName.Discrete.pH <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.pH)
ContData.env$myName.Discrete.Turbidity <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.Turbidity)
ContData.env$myName.Discrete.WaterP <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.WaterP)
ContData.env$myName.Discrete.WaterTemp <- paste(ContData.env$myPrefix.Discrete,ContData.env$myName.WaterTemp)

#--------------------------------------------------------------------
#Discrete Labels
ContData.env$myLab.Discrete.AirBP <- paste(ContData.env$myLab.AirBP,"(Discrete)")
ContData.env$myLab.Discrete.AirTemp <- paste(ContData.env$myLab.AirTemp,"(Discrete)")
ContData.env$myLab.Discrete.Chlorophylla <- paste(ContData.env$myLab.Chlorophylla,"(Discrete)")
ContData.env$myLab.Discrete.Cond <- paste(ContData.env$myLab.Cond,"(Discrete)")
ContData.env$myLab.Discrete.Discharge <- paste(ContData.env$myLab.Discharge,"(Discrete)")
ContData.env$myLab.Discrete.DO <- paste(ContData.env$myLab.DO,"(Discrete)")
ContData.env$myLab.Discrete.GageHeight <- paste(ContData.env$myLab.GageHeight,"(Discrete)")
ContData.env$myLab.Discrete.pH <- paste(ContData.env$myLab.pH,"(Discrete)")
ContData.env$myLab.Discrete.Turbidity <- paste(ContData.env$myLab.Turbidity,"(Discrete)")
ContData.env$myLab.Discrete.WaterP <- paste(ContData.env$myLab.WaterP,"(Discrete)")
ContData.env$myLab.Discrete.WaterTemp <- paste(ContData.env$myLab.WaterTemp,"(Discrete)")

#--------------------------------------------------------------------
# Automated QC stuff ####
## data type/stages
ContData.env$myDataQuality.Raw <- "RAW"
ContData.env$myDataQuality.QCauto <- "QCauto"
ContData.env$myDataQuality.QCmanual <- "QCmanual"
ContData.env$myDataQuality.Final <- "Final"
ContData.env$myDataQuality.Aggregated <- "Aggregated"

#--------------------------------------------------------------------
# Directory Names ####
ContData.env$myName.Dir.0Original <- "Data0_Original"
ContData.env$myName.Dir.1Raw <- "Data1_Raw"
ContData.env$myName.Dir.2QC <- "Data2_QC"
ContData.env$myName.Dir.3Agg <- "Data3_Aggregated"
ContData.env$myName.Dir.4Stats <- "Data4_Stats"

#--------------------------------------------------------------------
# Data Fields
ContData.env$myNames.DataFields <- c(
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
    )

ContData.env$myNames.DataFields.Lab <- c(
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
    )

ContData.env$myNames.DataFields.Col <- c(
      "blue","green","gray","gray","black","brown","purple","orange","salmon","rosybrown","aquamarine1"
    )

# Name Order (change order below to change order in output file)
ContData.env$myNames.Order <- c(
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
    )

#--------------------------------------------------------------------
## Data Quality Flag Values/nContData.env$myFlagVal.Pass <- "P"
ContData.env$myFlagVal.NotEval <- "NA"
ContData.env$myFlagVal.Suspect <- "S"
ContData.env$myFlagVal.Fail <- "F"
ContData.env$myFlagVal.NoData <- "X"
ContData.env$myFlagVal.Order <- c(
      ContData.env$myFlagVal.Pass, 
      ContData.env$myFlagVal.Suspect, 
      ContData.env$myFlagVal.Fail, 
      ContData.env$myFlagVal.NoData
    )

#--------------------------------------------------------------------
#Gross Fail Hi
ContData.env$myThresh.Gross.Fail.Hi.AirBP <- 
ContData.env$myThresh.Gross.Fail.Hi.AirTemp <- 
ContData.env$myThresh.Gross.Fail.Hi.Chlorophylla <- 
ContData.env$myThresh.Gross.Fail.Hi.Cond <- 
ContData.env$myThresh.Gross.Fail.Hi.Discharge <- 
ContData.env$myThresh.Gross.Fail.Hi.DO <- 
ContData.env$myThresh.Gross.Fail.Hi.GageHeight <- 
ContData.env$myThresh.Gross.Fail.Hi.pH <- 
ContData.env$myThresh.Gross.Fail.Hi.Turbidity <- 
ContData.env$myThresh.Gross.Fail.Hi.WaterP <- 
ContData.env$myThresh.Gross.Fail.Hi.WaterTemp <- 

#--------------------------------------------------------------------
#Gross Fail Lo
ContData.env$myThresh.Gross.Fail.Lo.AirBP <- 
ContData.env$myThresh.Gross.Fail.Lo.AirTemp <- 
ContData.env$myThresh.Gross.Fail.Lo.Chlorophylla <- 
ContData.env$myThresh.Gross.Fail.Lo.Cond <- 
ContData.env$myThresh.Gross.Fail.Lo.Discharge <- 
ContData.env$myThresh.Gross.Fail.Lo.DO <- 
ContData.env$myThresh.Gross.Fail.Lo.GageHeight <- 
ContData.env$myThresh.Gross.Fail.Lo.pH <- 
ContData.env$myThresh.Gross.Fail.Lo.Turbidity <- 
ContData.env$myThresh.Gross.Fail.Lo.WaterP <- 
ContData.env$myThresh.Gross.Fail.Lo.WaterTemp <- 

#--------------------------------------------------------------------
#Gross Suspect Hi
ContData.env$myThresh.Gross.Suspect.Hi.AirBP <- 
ContData.env$myThresh.Gross.Suspect.Hi.AirTemp <- 
ContData.env$myThresh.Gross.Suspect.Hi.Chlorophylla <- 
ContData.env$myThresh.Gross.Suspect.Hi.Cond <- 
ContData.env$myThresh.Gross.Suspect.Hi.Discharge <- 
ContData.env$myThresh.Gross.Suspect.Hi.DO <- 
ContData.env$myThresh.Gross.Suspect.Hi.GageHeight <- 
ContData.env$myThresh.Gross.Suspect.Hi.pH <- 
ContData.env$myThresh.Gross.Suspect.Hi.Turbidity <- 
ContData.env$myThresh.Gross.Suspect.Hi.WaterP <- 
ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp <- 

#--------------------------------------------------------------------
#Gross Suspect Lo
ContData.env$myThresh.Gross.Suspect.Lo.AirBP <- 
ContData.env$myThresh.Gross.Suspect.Lo.AirTemp <- 
ContData.env$myThresh.Gross.Suspect.Lo.Chlorophylla <- 
ContData.env$myThresh.Gross.Suspect.Lo.Cond <- 
ContData.env$myThresh.Gross.Suspect.Lo.Discharge <- 
ContData.env$myThresh.Gross.Suspect.Lo.DO <- 
ContData.env$myThresh.Gross.Suspect.Lo.GageHeight <- 
ContData.env$myThresh.Gross.Suspect.Lo.pH <- 
ContData.env$myThresh.Gross.Suspect.Lo.Turbidity <- 
ContData.env$myThresh.Gross.Suspect.Lo.WaterP <- 
ContData.env$myThresh.Gross.Suspect.Lo.WaterTemp <- 

#--------------------------------------------------------------------
#Spike Hi
ContData.env$myThresh.Spike.Hi.AirBP <- 
ContData.env$myThresh.Spike.Hi.AirTemp <- 
ContData.env$myThresh.Spike.Hi.Chlorophylla <- 
ContData.env$myThresh.Spike.Hi.Cond <- 
ContData.env$myThresh.Spike.Hi.Discharge <- 
ContData.env$myThresh.Spike.Hi.DO <- 
ContData.env$myThresh.Spike.Hi.GageHeight <- 
ContData.env$myThresh.Spike.Hi.pH <- 
ContData.env$myThresh.Spike.Hi.Turbidity <- 
ContData.env$myThresh.Spike.Hi.WaterP <- 
ContData.env$myThresh.Spike.Hi.WaterTemp <- 

#--------------------------------------------------------------------
#Spike Lo
ContData.env$myThresh.Spike.Lo.AirBP <- 
ContData.env$myThresh.Spike.Lo.AirTemp <- 
ContData.env$myThresh.Spike.Lo.Chlorophylla <- 
ContData.env$myThresh.Spike.Lo.Cond <- 
ContData.env$myThresh.Spike.Lo.Discharge <- 
ContData.env$myThresh.Spike.Lo.DO <- 
ContData.env$myThresh.Spike.Lo.GageHeight <- 
ContData.env$myThresh.Spike.Lo.pH <- 
ContData.env$myThresh.Spike.Lo.Turbidity <- 
ContData.env$myThresh.Spike.Lo.WaterP <- 
ContData.env$myThresh.Spike.Lo.WaterTemp <- 

#--------------------------------------------------------------------
#RoC SD number
ContData.env$myThresh.RoC.SD.number.AirBP <- 
ContData.env$myThresh.RoC.SD.number.AirTemp <- 
ContData.env$myThresh.RoC.SD.number.Chlorophylla <- 
ContData.env$myThresh.RoC.SD.number.Cond <- 
ContData.env$myThresh.RoC.SD.number.Discharge <- 
ContData.env$myThresh.RoC.SD.number.DO <- 
ContData.env$myThresh.RoC.SD.number.GageHeight <- 
ContData.env$myThresh.RoC.SD.number.pH <- 
ContData.env$myThresh.RoC.SD.number.Turbidity <- 
ContData.env$myThresh.RoC.SD.number.WaterP <- 
ContData.env$myThresh.RoC.SD.number.WaterTemp <- 

#--------------------------------------------------------------------
#RoC SD period
ContData.env$myThresh.RoC.SD.period.AirBP <- 
ContData.env$myThresh.RoC.SD.period.AirTemp <- 
ContData.env$myThresh.RoC.SD.period.Chlorophylla <- 
ContData.env$myThresh.RoC.SD.period.Cond <- 
ContData.env$myThresh.RoC.SD.period.Discharge <- 
ContData.env$myThresh.RoC.SD.period.DO <- 
ContData.env$myThresh.RoC.SD.period.GageHeight <- 
ContData.env$myThresh.RoC.SD.period.pH <- 
ContData.env$myThresh.RoC.SD.period.Turbidity <- 
ContData.env$myThresh.RoC.SD.period.WaterP <- 
ContData.env$myThresh.RoC.SD.period.WaterTemp <- 

#--------------------------------------------------------------------
#Flat Hi
ContData.env$myThresh.Flat.Hi.AirBP <- 
ContData.env$myThresh.Flat.Hi.AirTemp <- 
ContData.env$myThresh.Flat.Hi.Chlorophylla <- 
ContData.env$myThresh.Flat.Hi.Cond <- 
ContData.env$myThresh.Flat.Hi.Discharge <- 
ContData.env$myThresh.Flat.Hi.DO <- 
ContData.env$myThresh.Flat.Hi.GageHeight <- 
ContData.env$myThresh.Flat.Hi.pH <- 
ContData.env$myThresh.Flat.Hi.Turbidity <- 
ContData.env$myThresh.Flat.Hi.WaterP <- 
ContData.env$myThresh.Flat.Hi.WaterTemp <- 

#--------------------------------------------------------------------
#Flat Lo
ContData.env$myThresh.Flat.Lo.AirBP <- 
ContData.env$myThresh.Flat.Lo.AirTemp <- 
ContData.env$myThresh.Flat.Lo.Chlorophylla <- 
ContData.env$myThresh.Flat.Lo.Cond <- 
ContData.env$myThresh.Flat.Lo.Discharge <- 
ContData.env$myThresh.Flat.Lo.DO <- 
ContData.env$myThresh.Flat.Lo.GageHeight <- 
ContData.env$myThresh.Flat.Lo.pH <- 
ContData.env$myThresh.Flat.Lo.Turbidity <- 
ContData.env$myThresh.Flat.Lo.WaterP <- 
ContData.env$myThresh.Flat.Lo.WaterTemp <- 

#--------------------------------------------------------------------
#Flat Tolerance
ContData.env$myThresh.Flat.Tolerance.AirBP <- 
ContData.env$myThresh.Flat.Tolerance.AirTemp <- 
ContData.env$myThresh.Flat.Tolerance.Chlorophylla <- 
ContData.env$myThresh.Flat.Tolerance.Cond <- 
ContData.env$myThresh.Flat.Tolerance.Discharge <- 
ContData.env$myThresh.Flat.Tolerance.DO <- 
ContData.env$myThresh.Flat.Tolerance.GageHeight <- 
ContData.env$myThresh.Flat.Tolerance.pH <- 
ContData.env$myThresh.Flat.Tolerance.Turbidity <- 
ContData.env$myThresh.Flat.Tolerance.WaterP <- 
ContData.env$myThresh.Flat.Tolerance.WaterTemp <- 

ContData.env$myThresh.Flat.MaxComp    <- max(
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
    )

#--------------------------------------------------------------------
# Data Fields with Flags
ContData.env$myName.Flag <- "Flag"
ContData.env$myNames.Cols4Flags <- c(ContData.env$myName.DateTime,ContData.env$myNames.DataFields)
ContData.env$myNames.Flags <- paste(ContData.env$myName.Flag,ContData.env$myNames.Cols4Flags)
ContData.env$myName.Flag.DateTime <- paste(ContData.env$myName.Flag,ContData.env$myName.DateTime)
ContData.env$myName.Flag.AirBP <- paste(ContData.env$myName.Flag,ContData.env$myName.AirBP)
ContData.env$myName.Flag.AirTemp <- paste(ContData.env$myName.Flag,ContData.env$myName.AirTemp)
ContData.env$myName.Flag.Chlorophylla <- paste(ContData.env$myName.Flag,ContData.env$myName.Chlorophylla)
ContData.env$myName.Flag.Cond <- paste(ContData.env$myName.Flag,ContData.env$myName.Cond)
ContData.env$myName.Flag.Discharge <- paste(ContData.env$myName.Flag,ContData.env$myName.Discharge)
ContData.env$myName.Flag.DO <- paste(ContData.env$myName.Flag,ContData.env$myName.DO)
ContData.env$myName.Flag.GageHeight <- paste(ContData.env$myName.Flag,ContData.env$myName.GageHeight)
ContData.env$myName.Flag.pH <- paste(ContData.env$myName.Flag,ContData.env$myName.pH)
ContData.env$myName.Flag.Turbidity <- paste(ContData.env$myName.Flag,ContData.env$myName.Turbidity)
ContData.env$myName.Flag.WaterP <- paste(ContData.env$myName.Flag,ContData.env$myName.WaterP)
ContData.env$myName.Flag.WaterTemp <- paste(ContData.env$myName.Flag,ContData.env$myName.WaterTemp)

#--------------------------------------------------------------------
# Data Quality Test Names/nContData.env$myNames.QCTests <- c("Gross","Spike","RoC","Flat")
ContData.env$myNames.QCCalcs <- c("SD.Time","SD","SDxN",paste("n",1:ContData.env$myThresh.Flat.MaxComp),"flat.Lo","flat.Hi"

)#--------------------------------------------------------------------
# Exceedance values for stats (default to Gross-Suspect-Hi value)/nContData.env$myExceed.WaterTemp  <- ContData.env$myThresh.Gross.Suspect.Hi.WaterTemp/nContData.env$myExceed.AirTemp    <- ContData.env$myThresh.Gross.Suspect.Hi.AirTemp/nContData.env$myExceed.SensorDepth <- ContData.env$myThresh.Gross.Suspect.Hi.SensorDepth/n/n#--------------------------------------------------------------------
# Date and Time Formats
ContData.env$myFormat.Date <- "%Y-%m-%d"
ContData.env$myFormat.Time <- "%H:%M:%S"
ContData.env$myFormat.DateTime <- "%Y-%m-%d %H:%M:%S"
ContData.env$DateRange.Start.Default <- format(as.Date("1900-01-01"),ContData.env$myFormat.Date)
ContData.env$DateRange.End.Default <- format(Sys.Date(),ContData.env$myFormat.Date)
# Time Zone, used in Gage script in dataRetrieval, OlsonNames()/nContData.env$myTZ <- Sys.timezone()/n/n#--------------------------------------------------------------------
# Time Frames (MM-DD)/nContData.env$myTimeFrame.Annual.Start <- "0101"
ContData.env$myTimeFrame.Annual.End <- "1231"
ContData.env$myTimeFrame.WaterYear.Start <- "1001"
#ContData.env$myTimeFrame.WaterYear.End <- "0930"
ContData.env$myTimeFrame.Season.Spring.Start <- "0301"
#ContData.env$myTimeFrame.Season.Spring.End <- "0531"
ContData.env$myTimeFrame.Season.Summer.Start <- "0601"
#ContData.env$myTimeFrame.Season.Summer.End <- "0831"
ContData.env$myTimeFrame.Season.Fall.Start <- "0901"
#ContData.env$myTimeFrame.Season.Fall.End <- "1130"
ContData.env$myTimeFrame.Season.Winter.Start <- "1201"
#ContData.env$myTimeFrame.Season.Winter.End <- "0228" #but 0229 in leap year, use start dates only

#--------------------------------------------------------------------
# Time Frame Names
ContData.env$myName.Yr <- "Year"
ContData.env$myName.YrMo <- "YearMonth"
ContData.env$myName.Mo <- "Month"
ContData.env$myName.MoDa <- "MonthDay"
ContData.env$myName.JuDa <- "JulianDay"
ContData.env$myName.Day <- "Day"
ContData.env$myName.Season <- "Season"
ContData.env$myName.YrSeason <- "YearSeason"

#--------------------------------------------------------------------
# for summary stats
ContData.env$myNames.Fields.TimePeriods <- c(
      ContData.env$myName.Yr,
      ContData.env$myName.YrMo,
      ContData.env$myName.Mo,
      ContData.env$myName.MoDa,
      ContData.env$myName.JuDa,
      ContData.env$myName.Season,
      ContData.env$myName.YrSeason
    )

#--------------------------------------------------------------------
# Exclude Trigger
# Trigger for Stats to exclude (TRUE) or include (FALSE) where flag = "fail"
ContData.env$myStats.Fails.Exclude <- TRUE  #FALSE #TRUE

#--------------------------------------------------------------------
# Report Format
ContData.env$myReport.Format <- "html"  # "html" or "docx" # DOCX requires Pandoc.
ContData.env$myReport.Dir <- file.path(system.file(package="ContDataQC"), "rmd")