#Toggle for sendsweetalert
sendask = TRUE

#UI for front page
output$aboutUI = renderUI({
  tagList(
    if (sendask == TRUE){
      sendSweetAlert(
        session = session,
        title = "User Agreement",
        text = "NALMS does not guarantee the results produced by this app. By using this app, the user agrees to take on all risks associated with its use.",
        type = "warning"
      )
    },
    tags$style(
      HTML("
           body {
            background-color: white;
            color: black;
           }

           .about-me {
            float: left;
            background-color: #367fa9;
            color: ghostwhite;
            text-align: center;
            width: 60%;
            border: 4px solid darkslategrey;
            padding: 8px;
           }
           .about-me-column {
            float: left;
            width: 20%;
            padding: 8px;
           }
           .outside-column {
            float: left;
            width: 25%;
            padding: 8px;
           }
           .details-column {
            float: left;
            width: 50%;
            background-color: ghostwhite;
            border: 2px solid darkslategrey;
            padding: 8px;
            font-size: large;
           }
           
           ")
    ),
    tags$div(
      class = "row",
      tags$div(
        class = "about-me-column"
      ),
      tags$div(
        class = "about-me",
        # style = "background-color: #367fa9; text-align: center; border-bottom: 4px solid slategrey;",
        tags$br(),
        tags$h1(
          style = "color: ghostwhite;",
          "Aquatic Ecological Logger Processing App"
        ),
        # tags$p(
        #   style = "color: ghostwhite;",
        #   "a product of the Regional Lake Monitoring Network"
        # ),
        tags$br()
      ),
      tags$div(
        class = "about-me-column"
      )
    ),
    tags$br(),
    tags$div(
      class = "row",
      tags$div(
        class = "outside-column"
      ),
      tags$div(
        class = "details-column",
        tags$p(
          "The Logger Processing App is an open source, collaborative project to provide a robust, yet easy-to-use interface for processing, 
      organizing, and vetting continuous environmental logger data. It has been developed by Tim Martin at the Minnesota Department of Natural
      Resources and is based on the ContDataQC R Package developed by Erik Leppo at Tetratech."
        )
      ),
      tags$div(
        class = "outside-column"
      )
    ),
    tags$br(),
    tabBox(
      id = "aboutbox",
      width = 12,
      tabPanel(
        title = "Sections Overview",
        uiOutput("sectionsoverviewUI")
      ),
      tabPanel(
        title = "Further Development",
        uiOutput("furtherdevUI")
      ),
      tabPanel(
        title = "Contacts",
        uiOutput("contactsUI")
      ),
      tabPanel(
        title = "Use",
        uiOutput("useUI")
      )
    )
  )
})

#UI for sections overview
output$sectionsoverviewUI = renderUI({
  tagList(
    tags$style(
      HTML(
        "
        body {
          background-color: white;
          color: black;
          }
          
        .sections-header {
          background-color: #3c8dbc;
          border: 2px solid darkslategrey;
          color: ghostwhite;
          font-size: large;
          padding: 8px;
          border-collapse: collapse;
         }
           
        .sections-name {
          background-color: ghostwhite;
          border: 1px solid darkslategrey;
          color: darkslategrey;
          font-size: large;
          padding: 8px;
        }
           
        .sections-text {
          background-color: ghostwhite;
          border: 1px solid darkslategrey;
          color: darkslategrey;
          font-size: medium;
          padding: 8px;
        }
        "
      )
    ),
    HTML("<CENTER>"),
    tags$br(),
    tags$br(),
    tags$table(
      tags$tr(
        tags$td(
          class = "sections-header",
          colspan = 2,
          tags$b(
            "Configuration"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Programs and Waterbodies"
        ),
        tags$td(
          class = "sections-text",
          "Upload a previously created configuration file. Add, Edit, or Delete Programs, Waterbodies, and Stations."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "QC Settings"
        ),
        tags$td(
          class = "sections-text",
          "Customize QC Threshold settings for each waterbody and parameter."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Logger File Definitions"
        ),
        tags$td(
          class = "sections-text",
          "Create profiles for input logger files, allowing the app to know which fields to process."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Export Options"
        ),
        tags$td(
          class = "sections-text",
          "Define how the final output data table will be organized and which support files to include."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Save Configuration File"
        ),
        tags$td(
          class = "sections-text",
          "Download the app configuration file which stores all of the user input settings."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-header",
          colspan = 2,
          tags$b(
            "Processing and QC"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Setup/Processing"
        ),
        tags$td(
          class = "sections-text",
          "Input the Logger Unit IDs and relative spatial location information at a specified Waterbody Station within a Program. Upload
          the raw data and process."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Visual QC"
        ),
        tags$td(
          class = "sections-text",
          "Interactively select and flag erroneous data that may not have been flagged automatically."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Data Summary"
        ),
        tags$td(
          class = "sections-text",
          "View the results of the QC processes."
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Export"
        ),
        tags$td(
          class = "sections-text",
          "Process the data into their final format and download."
        )
      )
    ),
    tags$br(),
    tags$br(),
    HTML("</CENTER>")
  )
})

#UI for contacts
output$contactsUI = renderUI({
  
  tagList(
    tags$style(
      HTML("
        body {
          background-color: white;
          color: black;
        }
     
        .contact-name {
          background-color: #3c8dbc;
          border: 1px solid darkslategrey;
          color: ghostwhite;
          font-size: large;
          padding: 8px;
        }
     
        .contact-info {
          background-color: ghostwhite;
          border: 1px solid darkslategrey;
          color: darkslategrey;
          font-size: large;
          padding: 6px;
        }
      ")
    ),
    HTML("<CENTER>"),
    tags$br(),
    tags$br(),
    tags$table(
      tags$tr(
        tags$td(
          class = "contact-name",
          "Tim Martin"
        ),
        tags$td(
          class = "contact-info",
          "Logger Processing App Developer"
        ),
        tags$td(
          class = "contact-info",
          tags$a(
            href = "mailto:tim.martin@state.mn.us",
            "tim.martin@state.mn.us"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "contact-name",
          "Erik Leppo"
        ),
        tags$td(
          class = "contact-info",
          "ContDataQC R Package Developer"
        ),
        tags$td(
          class = "contact-info",
          tags$a(
            href = "mailto:erik.leppo@tetratech.com",
            "erik.leppo@tetratech.com"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "contact-name",
          "Jen Stamp"
        ),
        tags$td(
          class = "contact-info",
          "Regional Monitoring Network Coordinator"
        ),
        tags$td(
          class = "contact-info",
          tags$a(
            href = "mailto:jen.stamp@tetratech.com",
            "jen.stamp@tetratech.com"
          )
        )
      )
    ),
    tags$br(),
    tags$br(),
    HTML("</CENTER>")
  )
})

#UI for use statement
output$useUI = renderUI({
  tagList(
    tags$style(
      HTML(
        "
        body {
          background-color: white;
          color: black;
         }
        .use-text {
            background-color: #f39c12;
            color: darkslategrey;
            font-size: medium;
            text-align: justified;
            width: 60%;
            border: 2px solid darkslategrey;
            padding: 8px;
        }
        "
      )
    ),
    tags$br(),
    HTML("<CENTER>"),
    tags$table(
      tags$tr(
        tags$td(
          class = "use-text",
          tags$p(
            "The developers do not guarantee the results generated by this application. The user assumes all risk associated with processing data with this tool."
          )
        )
      )
    ),
    tags$br(),
    HTML("</CENTER>")
  )
})

#UI for further development
output$furtherdevUI = renderUI({
  tagList(
    tags$style(
      HTML(
        "
        body {
          background-color: white;
          color: black;
          }
          
        .sections-header {
          background-color: #3c8dbc;
          border: 2px solid darkslategrey;
          color: ghostwhite;
          font-size: large;
          padding: 8px;
          border-collapse: collapse;
         }
           
        .sections-name {
          background-color: ghostwhite;
          border: 1px solid darkslategrey;
          color: darkslategrey;
          font-size: large;
          padding: 8px;
        }
           
        .sections-text {
          background-color: ghostwhite;
          border: 1px solid darkslategrey;
          color: darkslategrey;
          font-size: medium;
          padding: 8px;
        }
        "
      )
    ),
    HTML("<CENTER>"),
    tags$br(),
    tags$br(),
    tags$table(
      tags$tr(
        tags$td(
          class = "sections-header",
          colspan = 3,
          tags$b(
            "Configuration Updates"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "QC Settings"
        ),
        tags$td(
          class = "sections-text",
          "Develop stratified threshold settings based upon logger placement depth, latitude, and season. This will help minimize the number
          of false positives flagged by the automatic QC process."
        ),
        tags$td(
          class = "sections-text",
          "Priority High"
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-header",
          colspan = 3,
          tags$b(
            "Processing and QC Updates"
          )
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Setup/Processing"
        ),
        tags$td(
          class = "sections-text",
          "Allow the user to upload a .csv with pre-populated Unit IDs and Z values. This will remove the need to manually fill in the
          Logger Units Table"
        ),
        tags$td(
          class = "sections-text",
          "Priority Low"
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Discrete Data Comparison"
        ),
        tags$td(
          class = "sections-text",
          "Create a tool to upload discrete data, for example a Temperature/DO profile using a DO probe, to compare against the continuous data. Add
          an additional QC Flag and/or metadata section to relay the results of this comparison."
        ),
        tags$td(
          class = "sections-text",
          "Priority Medium"
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Previous Deployment Comparison"
        ),
        tags$td(
          class = "sections-text",
          "Check to see how the data from the previous deployment logger data at a station lines up with the processed data. Provide options for 
          addressing any discrepancies."
        ),
        tags$td(
          class = "sections-text",
          "Priority Medium"
        )
      ),
      tags$tr(
        tags$td(
          class = "sections-name",
          "Z Comparison"
        ),
        tags$td(
          class = "sections-text",
          "Ensure that the logger data are vertically consistent. For example, in a stratified lake, the deeper the logger is located, the 
          lower the temperature should be in relation to the loggers above it."
        ),
        tags$td(
          class = "sections-text",
          "Priority Medium"
        )
      )
    )
  )
})

#App Instructions
output$appinstruct = downloadHandler(
  filename = function(){
    "App_Instructions.pdf"
  },
  content = function(file){
    file.copy("docs/Instructions_v_draft.pdf",file)
  }
)