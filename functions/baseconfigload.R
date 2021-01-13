#Load Configuration data file
load("config/baseconfig.RData")

loggerchoices = list("Air Pressure"="AirBP","Air Temperature"="AirTemp","Chlorophyll A"="Chlorophylla","Conductivity"="Cond","Discharge"="Discharge",
                     "Dissolved Oxygen"="DO","Gage Height"="GageHeight","pH"="pH","Turbidity"="Turbidity","Water Pressure"="WaterP",
                     "Water Temperature"="WaterTemp")

#Assign programs data frame to a reactive value
programs = reactiveVal(baseconfig$programs)
#Assign program waterbodies data frame to a reactive value
programwbs = reactiveVal(baseconfig$programwbs)
#Assign waterbody names data frame to a reactive value
wbnames = reactiveVal(baseconfig$wbnames)
#Assign stations data frame to a reactive value
stations = reactiveVal(baseconfig$stations)
#Assign processing logs data frame to a reactive value
processinglogs = reactiveVal(baseconfig$processinglogs)
#Assign QC configuration settings data frame to a reactive value
qc_config = reactiveVal(baseconfig$qc_config)
#Assign Logger File definitions to a reactive value
loggerfiledefs = reactiveVal(baseconfig$loggerfiledefs)
#Assign Deploy Logs to a reactive value
deploylogs = reactiveVal(baseconfig$deploylogs)
#Assign Export Settings to a reactive value
export = reactiveVal(baseconfig$export)

#Update base config tables and save to file
updatebaseconfig = function(){
  baseconfig$programs = programs()
  baseconfig$programwbs = programwbs()
  baseconfig$wbnames = wbnames()
  baseconfig$stations = stations()
  baseconfig$processinglogs = processinglogs()
  baseconfig$qc_config = qc_config()
  baseconfig$loggerfiledefs = loggerfiledefs()
  baseconfig$deploylogs = deploylogs()
  baseconfig$export = export()
  save(baseconfig,file = "config/baseconfig.RData")
}