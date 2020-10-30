library(ids)
getwd()
#ProgramIDs - 6 bytes
#AppIDs - 8 bytes

#Define Programs Table
programs=data.frame("ProgramID" = "cd4cba49cd35","Program_Name" = "Minnesota Sentinel Lakes",stringsAsFactors = FALSE)

#Define Waterbodies Table
programwbs=data.frame(
  "ProgramID" = "cd4cba49cd35",
  "ProgramWaterbodyID" = mnsentinellakes::sentinellakesmetadata$LakeId,
  "AppID" = c("5095adf462791e66","1fd05dfa4455cab4","699161cea78471c1","b682ff85725353d0","26df0000cd20a19b","d7123440306bda5a","fcf4e38c8e5ce51b",
              "616a11390a7dfd5c","6f6a51b6bc6bfc08","b42738f95726b801","8b344c8fc841aa88","060e6635ba61e764","94b8f9392de0e1e4","eee0004fab488142",
              "0b5b5fa17e292943","465d2a6a91b0e8d7","04dba61814935855","a9bee292e0bfdb30","9bdb6858cac527ca","69e9d3e395c46a02","1e91b2f7caad6893",
              "73280c0f065388e9","9a4bf92c5101e10c","4ef33316ac29d54e","572f8736891678a8"),
  "WB_Type" = c("Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake","Lake",
                "Lake","Lake","Lake","Lake","Lake","Lake","Lake"),
  stringsAsFactors = FALSE
)

#Define Waterbodies names table
wbnames=data.frame(
  "AppID"= c("5095adf462791e66","1fd05dfa4455cab4","699161cea78471c1","b682ff85725353d0","26df0000cd20a19b","d7123440306bda5a","fcf4e38c8e5ce51b",
              "616a11390a7dfd5c","6f6a51b6bc6bfc08","b42738f95726b801","8b344c8fc841aa88","060e6635ba61e764","94b8f9392de0e1e4","eee0004fab488142",
             "0b5b5fa17e292943","465d2a6a91b0e8d7","04dba61814935855","a9bee292e0bfdb30","9bdb6858cac527ca","69e9d3e395c46a02","1e91b2f7caad6893",
             "73280c0f065388e9","9a4bf92c5101e10c","4ef33316ac29d54e","572f8736891678a8"),
  "Waterbody_Name" = mnsentinellakes::sentinellakesmetadata$Lake,
  stringsAsFactors = FALSE
)

#Define Waterbody Stations
stations = data.frame(
  "AppID" = "b42738f95726b801",
  "StationID" = random_id(n = 1,bytes = 12),
  "Station_Name" = "Primary",
  "Lat"=NA,
  "Lon"=NA,
  stringsAsFactors = FALSE
)

#Define waterbody logger types
##Deprecated, but still used to build QC Config Table
wbloggertypes=data.frame(
  "AppID" = c("5095adf462791e66","1fd05dfa4455cab4","699161cea78471c1","b682ff85725353d0","26df0000cd20a19b","d7123440306bda5a","fcf4e38c8e5ce51b",
             "616a11390a7dfd5c","6f6a51b6bc6bfc08","b42738f95726b801","8b344c8fc841aa88","060e6635ba61e764","94b8f9392de0e1e4","eee0004fab488142",
             "0b5b5fa17e292943","465d2a6a91b0e8d7","04dba61814935855","a9bee292e0bfdb30","9bdb6858cac527ca","69e9d3e395c46a02","1e91b2f7caad6893",
             "73280c0f065388e9","9a4bf92c5101e10c","4ef33316ac29d54e","572f8736891678a8"),
  "Loggers" = c("WaterTemp;DO","WaterTemp","WaterTemp","WaterTemp;DO","WaterTemp","WaterTemp","WaterTemp","WaterTemp","WaterTemp;DO","WaterTemp;DO",
                "WaterTemp","WaterTemp;DO","WaterTemp","WaterTemp","WaterTemp","WaterTemp","WaterTemp","WaterTemp;DO","WaterTemp","WaterTemp",
                "WaterTemp","WaterTemp","WaterTemp;DO","WaterTemp","WaterTemp;DO"),
  stringsAsFactors = FALSE
)

#Define Logger Field Definitions
loggerfiledefs = read.csv("C:/Projects/Shiny_App_Development/Logger_Processing/setup/loggers.csv",stringsAsFactors = FALSE)

for (i in 2:18){
  loggerfiledefs[,i] = as.character(loggerfiledefs[,i])
  
  
}

loggerfiledefs$ModelID = random_id(n = 2, bytes = 6)
# loggerfiledefs=loggerfiledefs[,c(1:2,17,3:16)]
# loggerfiledefs$TZ = "UTC"

#Define Individual Logger Processing Logs

processinglogs = data.frame("Serial_Number" = as.character(NA),"Depth" = as.numeric(NA),"Processed" = as.Date(as.character(NA)),
                            "ModelID" = as.character(NA),"StationID" = as.character(NA),"DeployID" = as.character(NA),
                            "ProcID" = random_id(n=1,bytes = 12),stringsAsFactors = FALSE)



#Define Deployments

# depcounts = unique(data.frame("DeployID"=processinglogs$DeployID,"Deployment"=processinglogs$Deployment,stringsAsFactors = FALSE))
# 
# deploylogs = data.frame("DeployID" = unique(processinglogs$DeployID),"Site" = "Primary","Lat" = NA, "Lon" = NA,
# "StartDate"=NA,"EndDate"=NA,stringsAsFactors = FALSE)
# deploylogs$Units = c("C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","mg/L","mg/L","mg/L","mg/L","mg/L")
# deploylogs$Logger_Count = NA
# deploylogs = left_join(deploylogs,depcounts)

deploylogs = data.frame("DeployID" = as.character(NA),"Logger_Type" = as.character(NA),"Lat" = as.numeric(NA),"Lon" = as.numeric(NA),
                        "StartDateTimeRecord" = as.character(NA),
                        "EndDateTimeRecord" = as.character(NA),"StartDateTimeValid" = as.character(NA),
                        "EndDateTimeValid" = as.character(NA),"Units" = as.character(NA),"Logger_Count" = as.numeric(NA),
                        "Deployment_Count" = as.numeric(NA),"ProcessedDate" = as.character(NA),"Processedby" = as.character(NA),
                        stringsAsFactors = FALSE)


#Add Units to deploylogs

# processinglogs$Deployment = NULL

#Define QC Config Table
options(scipen = 999)
qc_config = read.csv("C:/Projects/Shiny_App_Development/Logger_Processing/setup/QC_config.csv",stringsAsFactors = FALSE)

qc_config$AppID=as.character(1111111111111111)
qc_config$Value=as.numeric(qc_config$Value)

qc_config_addappid=NULL
for (i in wbloggertypes$AppID){
  qc_config_add=qc_config
  qc_config_add$AppID=i
  qc_config_addappid=rbind(qc_config_addappid,qc_config_add)
  
  
}
qc_config=rbind(qc_config,qc_config_addappid)


#Create list of all tables
baseconfig=list("programs"=programs,"programwbs"=programwbs,"wbnames"=wbnames,"stations"=stations,"processinglogs"=processinglogs,
                "qc_config"=qc_config,"loggerfiledefs"=loggerfiledefs,"deploylogs"=deploylogs)

#Save all tables
save(baseconfig,file = "C:/Projects/Shiny_App_Development/Logger_Processing/config/baseconfig.RData")

