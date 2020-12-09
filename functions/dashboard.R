# #Dropdown Blocks----
# #Dropdown menus on the head for selecting the lake and logger type
# dropdownmenus=function(id){
#   tagList(
#     dropdownBlock(
#       id = paste0(id,"1"),
#       title = "Lake Name",
#       textInput(
#         inputId = "lakeinput",
#         label="",
#         placeholder = "Lake Name"
#       ),
#       #Submit Lake Name----------------------
#       actionBttn(
#         inputId = "lakeapply",
#         label="Submit",
#         color="success",
#         style = "fill",
#         size = 'sm'
#       )
#     ),
#     dropdownBlock(
#       id = paste0(id,"2"),
#       title = "Logger Type",
#       # icon = "temperature-high",
#       awesomeRadio(
#         inputId = "Type",
#         label = "",
#         choices = c("Temperature","Dissolved Oxygen"),
#         status = "success"
#       )
#     )
#   )
# }

#Sidebar----
logprocsidebar=function(id){
  dashboardSidebar(
    
    width = 275,
    HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>"),
    #Processing steps, to be followed sequentially----
    sidebarMenu(
      id=id,
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Configuration",
        icon = icon("sliders-h"),
        startExpanded = TRUE,
        menuSubItem("Programs and Waterbodies",tabName = "programsandwaterbodies"),
        menuSubItem("QC Settings",tabName = "configqc"),
        menuSubItem("Logger File Definitions",tabName = "logfiledefs"),
        menuSubItem("Export Options",tabName = "exportoptions"),
        menuSubItem("Save Configuration File",tabName = "saveconfigfile")
      ),
      menuItem(
        "Processing and QC", 
        icon = icon("laptop"),
        menuSubItem("Setup/Processing",tabName = "setup"),
        menuSubItem("Visual QC",tabName = "visqc"),
        menuSubItem("Data Summary",tabName = "summary"),
        menuSubItem("Export",tabName = "export")
      )
    )
  )
}