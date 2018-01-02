
#UI function to arrange shiny front end

ui <- shinydashboard::dashboardPage(
    skin = "red",
    shinydashboard::dashboardHeader(
      title = "PEO ACWA eMeasure"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "Data Upload",
          tabName = "data_entry",
          icon = shiny::icon("upload")
        ),
        shinydashboard::menuItem(
          "HQ Raw Data",
          icon = shiny::icon("dashboard"),
          tabName = "HQrawdata"
        ),
        shinydashboard::menuItem(
          "Pueblo Raw Data",
          icon = shiny::icon("dashboard"),
          tabName = "pueblorawdata"
        ),
        shinydashboard::menuItem(
          "Blue Grass Raw Data",
          icon = shiny::icon("dashboard"),
          tabName = "bluegrassrawdata"
        ),
        shinydashboard::menuItem(
          "Dashboard",
          icon = shiny::icon("dashboard"),
          tabName = "dashboard"
        )
      )
    ),
shinydashboard::dashboardBody(
  tags$head(
    tags$link(rel = "icon", 
              type = "image/png", 
              href = "myimage.png"),
    tags$title("PEO ACWA eMeasure")
  ), 
  shinydashboard::tabItems(
shinydashboard::tabItem(
  tabName = "data_entry",
  shiny::fluidRow(
    width = 12, 
    align = 'left',
    column( width = 12,
            img(src='myimage.png', align = "left", width = 500, height = 300),
  radioButtons("site", label = h3("1. Location:"),
               choices = list("Headquarters", "Pueblo", "Blue Grass")),

  uiOutput("survey")


)
)
),

shinydashboard::tabItem(tabName = "HQrawdata",
                        textOutput("testprint"),
                        box(width = 12,
                            h3("HQ Individual Interactions"),
                            #actionButton("deleteRows0", "Delete Rows"),actionButton("cancel0", "Undo last action"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("HQIIrawdata")),
                            downloadButton("downloadHQII", "Download")
                            #, 
                            #actionButton("saveHQII", "Save") 
                            #actionButton("loadHQII", "Load previous data")
                        ),
                        box(width = 12,
                            h3("HQ Event or Distributions"),
                            #actionButton("deleteRows1", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("HQEDrawdata")), 
                            downloadButton("downloadHQED", "Download")
                        ),
                        box(width = 12,
                            h3("HQ Social Media or Web"),
                           #actionButton("deleteRows2", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("HQSMSrawdata")), 
                            downloadButton("downloadHQSMS", "Download")
                        ), 
                        actionButton("saveHQall", "Save and Apply to Dashboard"),
                       actionButton("downloadHQRAW", "Re-Download edited raw data")
),

shinydashboard::tabItem(tabName = "pueblorawdata",
                        box(width = 12,
                            h3("Pueblo Individual Interactions"),
                            #actionButton("deleteRows3", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("PBIIrawdata")), 
                            downloadButton("downloadPBII", "Download")
                        ), 
                        box(width = 12, 
                            h3("Pueblo Event or Distributions"),
                            #actionButton("deleteRows4", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("PBEDrawdata")), 
                            downloadButton("downloadPBED", "Download")
                        ), 
                        box(width = 12, 
                            h3("Pueblo Social Media or Web"),
                            #actionButton("deleteRows5", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("PBSMSrawdata")), 
                            downloadButton("downloadPBSMS", "Download")
                        ), 
                        actionButton("savePBall", "Save and Apply to Dashboard"), 
                        actionButton("downloadPBRAW", "Re-Download edited raw data")
),

shinydashboard::tabItem(tabName = "bluegrassrawdata",
                        box(width = 12,
                            h3("Blue Grass Individual Interactions"),
                            #actionButton("deleteRows6", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("BGIIrawdata")), 
                            downloadButton("downloadBGII", "Download")
                        ), 
                        box(width = 12, 
                            h3("Blue Grass Event or Distributions"),
                            #actionButton("deleteRows7", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("BGEDrawdata")), 
                            downloadButton("downloadBGED", "Download")
                        ), 
                        box(width = 12, 
                            h3("Blue Grass Social Media or Web"),
                            #actionButton("deleteRows8", "Delete Rows"),
                            div(style = 'overflow-x: scroll', rHandsontableOutput("BGSMSrawdata")), 
                            downloadButton("downloadBGSMS", "Download")
                        ), 
                        actionButton("saveBGall", "Save and Apply to Dashboard"),
                        actionButton("downloadBGRAW", "Re-Download edited raw data")
                        
),

shinydashboard::tabItem(tabName = "dashboard",
                        h2("Headquarters:"),
                        dataTableOutput('HQtable1'),
                        h2("Pueblo:"),
                        dataTableOutput('PBtable1'),
                        h2("Blue Grass:"),
                        dataTableOutput('BGtable1'), 
                        h2("Feedback:"),
                        dataTableOutput('Feedbacktable1'),
                        downloadButton("report", "Generate report")

)
)))




#shinyApp(ui, server)

