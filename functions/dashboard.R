#Sidebar----
logprocsidebar=function(id){
  dashboardSidebar(
    width = 275,
    HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>"),
    #Processing steps, to be followed sequentially----
    sidebarMenu(
      id=id,
      menuItem(
        "About",
        tabName = "abouttab",
        icon = icon("home")
      ),
      menuItem(
        "Configuration",
        icon = icon("sliders-h"),
        startExpanded = TRUE,
        menuSubItem("Programs and Waterbodies",tabName = "programsandwaterbodiestab"),
        menuSubItem("QC Settings",tabName = "configqctab"),
        menuSubItem("Logger File Definitions",tabName = "logfiledefstab"),
        menuSubItem("Export Options",tabName = "exportoptionstab"),
        menuSubItem("Save Configuration File",tabName = "saveconfigfiletab")
      ),
      menuItem(
        "Processing and QC", 
        icon = icon("laptop"),
        menuSubItem("Setup/Processing",tabName = "setuptab"),
        menuSubItem("Visual QC",tabName = "visqctab"),
        menuSubItem("Data Summary",tabName = "summarytab"),
        menuSubItem("Export",tabName = "exporttab")
      )
    )
  )
}