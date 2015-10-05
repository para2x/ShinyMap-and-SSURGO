## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)

 dashboardPage(
   ##################
   #####CSS
   ##################
 
   #################
   ##### HEADER
   ################
   dashboardHeader(title = "Dashboard"),
   
   
   #########################################
   ## Sidebar content
   ############################################
   dashboardSidebar(
     sidebarMenu(
       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
       menuItem("Soil database", tabName = "SoilDB", icon = icon("th"))
     )
   ),
   ################
   ##################################### BODY
   ################
   dashboardBody(
     tags$head(tags$style("
                       #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
                       .floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
                       ")),
     tabItems(
       # First tab content
       tabItem(tabName = "dashboard",
               fluidRow(
                 h2("Welcome to my interactive soil database app",align="center"),
                 br(),
                 h3("Designed and developed by Hamze Dokoohaki",align="center"),
                 br(),
                 h3("Agro-ecosystems Modeling Lab",align="center"),
                 br(),
                 h3("Iowa State University",align="center")
               )
       ),
# end of tab 2
     # Soil DB
     tabItem(tabName = "SoilDB",
             fluidRow(
               column(width = 12,
                      box(
                        title = "Soil Maps", width = 12, solidHeader = TRUE, status = "primary",
                        
                        leafletOutput("map", height = 400),
                        
                        absolutePanel(
                          right = 30, top = 60, width = 250, class = "floater",
                          
                          h4("Info"),
                          h4(htmlOutput("stateInfo"))
                        ),
                        br()
                      ),
                      box(
                        title = "Data", width = 12, solidHeader = TRUE, status = "primary",
                        dataTableOutput(outputId="table"),
                        conditionalPanel("input.map_click != null",
                            downloadButton('downloadData', 'Download')
                          )
                      ),
                      box(
                        title = "Soil Analysis", width = 12, solidHeader = TRUE, status = "primary",
                        conditionalPanel("input.map_click != null",
                          selectInput("field", "Choose a field to plot:", 
                                      choices = c("OM", "SAND", "SILT","CLAY","AWC"))
                          ),
                        plotOutput("anaplot",width = "100%", height="auto")
                      )
               )
             )
     )# end of Soil DB
   ) ## end of tab pages
)
)
