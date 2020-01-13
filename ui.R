

setwd("C:/R_Shiny_Dashboard")

library(shiny)
library(shinydashboard)
library(magrittr)



# || 1. HEADER -----------------------------------------------------------------------

header = dashboardHeader(title = "Spatially Adjusted Time-varying Reproductive Numbers", 
                         titleWidth = 530, 
                         dropdownMenu(type = "messages",
                                                        messageItem(
                                                          from = "Sales Dept",
                                                          message = "Sales are steady this month."
                                                        ),
                                                        messageItem(
                                                          from = "New User",
                                                          message = "How do I register?",
                                                          icon = icon("question"),
                                                          time = "13:45"
                                                        ),
                                                        messageItem(
                                                          from = "Support",
                                                          message = "The new server is ready.",
                                                          icon = icon("life-ring"),
                                                          time = "2014-12-01"
                                                        )
                                      ), #dropdownMenu_messages
                         
                         dropdownMenu(type = "notifications",
                                      notificationItem(
                                        text = "5 new users today",
                                        icon("users")
                                      ),
                                      notificationItem(
                                        text = "12 items delivered",
                                        icon("truck"),
                                        status = "success"
                                      ),
                                      notificationItem(
                                        text = "Server load at 86%",
                                        icon = icon("exclamation-triangle"),
                                        status = "warning"
                                      )
                         ), #dropdownMenu_notifications
                         
                         dropdownMenu(type = "tasks", badgeStatus = "success",
                                      taskItem(value = 90, color = "green",
                                               "Documentation"
                                      ),
                                      taskItem(value = 17, color = "aqua",
                                               "Project X"
                                      ),
                                      taskItem(value = 75, color = "yellow",
                                               "Server deployment"
                                      ),
                                      taskItem(value = 80, color = "red",
                                               "Overall project"
                                      )
                         )  #dropdownMenu_tasks
                         
                         
                         ) #Header


# || 2. SIDEBAR ---------------------------------------------------------------------------------

sidebar = dashboardSidebar(
  sidebarMenu(
    
    # __2.1 Descriptive -----------------------------------------------------------------
              menuItem("Disease Overview", tabName = "descriptive", icon = icon("globe"), 
                       menuSubItem("Disease Dashbaord", tabName = "descriptive", icon = icon("dashboard")), 
                         fileInput("csv", label = "[Text file required]", 
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
                         checkboxInput("header", strong("Header"), TRUE),
                         
                         radioButtons("sep", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t"),
                                      selected = ","),
                         
                         radioButtons("quote", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = '"')
                       #)
                       ),
              
              ###
              # fileInput("csv", label = strong("File input:"), 
              #           multiple = FALSE,
              #           accept = c("text/csv",
              #                      "text/comma-separated-values,text/plain",
              #                      ".csv")),
              # 
              # checkboxInput("header", strong("Header"), TRUE),
              # 
              # radioButtons("sep", "Separator",
              #              choices = c(Comma = ",",
              #                          Semicolon = ";",
              #                          Tab = "\t"),
              #              selected = ","),
              # 
              # radioButtons("quote", "Quote",
              #              choices = c(None = "",
              #                          "Double Quote" = '"',
              #                          "Single Quote" = "'"),
              #              selected = '"'),
              ###
              
              # Input: Select number of rows to display ----
              # radioButtons("disp", "Display",
              #              choices = c(Head = "head",
              #                          All = "all"),
              #              selected = "head"),
              
    # __2.2 Rj -----------------------------------------------------------------
              menuItem("Rj Dashboard", icon = icon("dashboard"), tabName = "rj", badgeLabel = "new", badgeColor = "green"), 
              
    # __2.3 More... -----------------------------------------------------------------          
              menuItem("More...", tabName = "appendix", icon = icon("th"))
              
              )
  
  
  
)

# || 3. BODY -------------------------------------------------------------------------------
body = dashboardBody(
  tabItems(
    
    # __3.1 Descriptive -----------------------------------------------------------------
    tabItem(tabName = "descriptive",
            fluidRow(
              column(width = 4, 
                     fluidRow(
                       column(12,
                              box(width = 13, 
                                  height = 300, status="primary",
                                  title = tagList(shiny::icon("info-circle"), strong("Disease info")),
                                  
                                  fluidRow(tags$head(tags$style(HTML('.info-box {min-height: 100px; width: 420px} .info-box-icon {height: 100px; line-height: 100px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}')),
                                                     tags$style(HTML("input[type=\"number\"] {width: 100px;}"))
                                                     ),
                                           valueBox("Dengue", "Type of Disease", icon = icon("dizzy"), width = 12, color = "blue")
                                  ),
                                  fluidRow(#infoBox("disease_count_test", "Count of disease", icon = icon("credit-card")), #testing layout
                                    valueBoxOutput("disease_count", width = 12)
                                  )
                              )
                       )
                     ), 
                     
                     fluidRow(
                       column(12,

                              box(width = 13,
                                  height = 480, status="success",
                                  title = tagList(shiny::icon("poll"), strong("Epidemic curve")),
                                  #title = strong("Epidemic curve"),

                                  # test start --------------------------------

                                  #verbatimTextOutput("textt"),
                                  #tableOutput("contents")

                                  # test end ----------------------------------
                                  plotOutput("epi_curve"))
                              
                              
                       )
                     )
              ),
              
              column(width = 8, 
                     
                     tabBox(width = 24, height = 800, 
                            #status = "danger", collapsible = TRUE,
                            
                       # Title can include an icon
                       title = tagList(shiny::icon("map-marked-alt"), strong("Spatial pattern")),
                       
                       tabPanel("Point", 
                                #status = "danger", collapsible = TRUE,
                                plotOutput("Sp_point")),
                       
                       tabPanel("Hexagon", 
                                sliderInput("Sp_hexagon_slider", "Number of bin:", 1, 100, 50),
                                plotOutput("Sp_hexagon")), 
                       
                       tabPanel("Kernal", 
                                sliderInput("Sp_kde_slider", "Number of grid:", 1, 500, 200),
                                plotOutput("Sp_kde"))
                     )
                     
                     # box(width = 24, 
                     #     height = 800, status = "danger", collapsible = TRUE,
                     #     infoBox("Static map", 10 * 2, icon = icon("credit-card"))), 
                     #box(width = 24, height = 800, status = "warning", collapsible = TRUE, collapsed = TRUE)
                     
                     
              )
            ) #fluidRow
      
    ),
    
    # __3.2 Rj ---------------------------------------------------------------------
    
    tabItem(tabName = "rj", 
      fluidRow(
        column(width = 3, 
               box(width = 13, 
                   height = 570, status="primary", collapsible = TRUE,
                   title = tagList(shiny::icon("gear"), strong("Settings")),
                   
                   h4(strong("Generation interval")), 
                   h5(strong("Gamma distribution")),
                   fluidRow(
                     column(width = 4,
                            numericInput("gamma_mean", label = p(strong("- Mean")), value = 20)
                           ), 
                     column(width = 4, 
                            numericInput("gamma_variance", label = p(strong("- Variance")), value = 9)
                           ), 
                     column(width = 4,
                            br(),
                            br(),
                            checkboxInput("Log1", label = "Log?", value = TRUE)
                           )
                   ),
                   
                   hr(),
                   h4(strong("Spatial weighting function")), 
                   h5(strong("Exponential distribution")),
                   
                   fluidRow(
                     column(width = 8,
                            numericInput("dist", label = p(strong("- Mean transmission distance")), value = 125)
                     ), 
                    
                     column(width = 4,
                            br(),
                            br(),
                            checkboxInput("Log2", label = "Log?", value = TRUE)
                     )
                   ), 
                   
                   hr(), 
                   checkboxInput("sp_adjust", label = strong("Spatial-adjusted?"), value = TRUE),
                   actionButton("rj_go", label = "Run"),
                   #verbatimTextOutput("action_test")
                   
               )
        ), #column 1: width = 3
        
        column(width = 4, 
               
               tabBox(width = 13, height = 570, 
                      #status = "danger", collapsible = TRUE,
                      
                      # Title can include an icon
                      title = tagList(shiny::icon("map-marked-alt"), strong("Spatial Pattern of Rj")),

                      tabPanel("Point", 
                               fluidRow(
                                 column(width = 6,
                                        dateRangeInput("dates", label = strong("Date range: "), 
                                                       start  = "2007-09-15",
                                                       end    = "2007-10-15")
                                        )
                               ),

                               plotOutput("Rj_pnt")),
                      
                      tabPanel("Hexagon", 
                               fluidRow(
                                 column(width = 6,
                                        dateRangeInput("dates", label = strong("Date range: "), 
                                                       start  = "2007-09-15",
                                                       end    = "2007-10-15")
                                        ), 
                                 column(width = 6, 
                                        sliderInput("Rj_hexagon_slider", "Number of bin:", 1, 100, 50)
                                        )
                               ),
                               
                               plotOutput("Rj_hexagon")) 
                      
               )
               ), 
        
        column(width = 5, 
               
               tabBox(width = 13, height = 570, 
                      #status = "danger", collapsible = TRUE,
                      
                      # Title can include an icon
                      title = tagList(shiny::icon("history"), strong("Spatio-temporal Pattern")),
                      
                      tabPanel("Point", 
                               imageOutput("Rj_spt_ptrn_point")),
                      
                      tabPanel("Hexagon", 

                               sliderInput("Rj_hexagon_slider2", "Number of bin:", 1, 100, 50),
                               
                               imageOutput("Rj_spt_ptrn_hexagon")) 
                      
               )
               )

      ), #fluidRow 1
      fluidRow(
        
        column(width = 3, 
               box(width = 13, height = 450, status="danger", collapsible = TRUE,
                   title = tagList(shiny::icon("poll-h"), strong("Summary of Rj")), 
                   
                   fluidRow(
                     #valueBox(20, "Min Rj", icon = icon("exclamation-circle"), color= "blue", width = 6),
                     #valueBox(20, "Max Rj", icon = icon("exclamation-circle"), color= "red", width = 6)
                     valueBoxOutput("rj_box_min", width = 6), 
                     valueBoxOutput("rj_box_max", width = 6)
                   ),
                   
                   fluidRow(
                     #valueBox(20, "Median Rj", icon = icon("exclamation-circle"), color= "yellow", width = 12)
                     valueBoxOutput("rj_box_med", width = 12)
                   ),
                   
                   fluidRow(
                     column(width = 3, 
                            h5(strong("- Q1:")), 
                            h5(strong("- Q3:")), 
                            h5(strong("- Mean:"))
                            ), 
                     column(width = 4, 
                            h5(textOutput("rj_q1")), 
                            h5(textOutput("rj_q3")), 
                            h5(textOutput("rj_mean"))
                            )
                   )
                   
                   # HERE _________________________________

                   # fluidRow(
                   #   valueBoxOutput("rj_box_mean")
                   # )
                   
                   )
               ), 
        column(width = 9, 
               box(width = 13, height = 450, status="danger", collapsible = TRUE,
                   title = tagList(shiny::icon("poll"), strong("Time-varying reproductive numbers")), 
                   
                   plotOutput("Rj_time")
                   
                   )
               )
        
      ) #fluidRow 2
      
      
      
    ) #NEWWWWWW
    
    
    
    # tabItem(tabName = "rj", 
    #         fluidRow(
    #           column(width = 4, 
    #                  fluidRow(
    #                    column(12,
    #                           box(width = 13, 
    #                               height = 300, status="primary", 
    #                               title = strong("Settings"),
    #                               
    #                               strong("Define generation interval: Gamma distribution"), 
    #                               numericInput("gamma_mean", label = p(strong("Mean")), value = 20), 
    #                               numericInput("gamma_variance", label = p(strong("Variance")), value = 9), 
    #                               checkboxInput("Log1", label = "Log?", value = TRUE),
    #                               
    #                               strong("Define spatial weighting function: Exponential distribution"),
    #                               numericInput("dist", label = p(strong("Mean transmission distance")), value = 125), 
    #                               checkboxInput("Log2", label = "Log?", value = TRUE)
    #                             
    #                           )
    #                    )
    #                  ), 
    #                  
    #                  fluidRow(
    #                    column(12,
    #                           dateRangeInput("dates", label = strong("Date range: "), 
    #                                                 start  = "2007-09-15",
    #                                                 end    = "2007-10-15"),
    #                           
    #                           tabBox(width = 13, height = 480, 
    #                                  #status = "danger", collapsible = TRUE,
    #                                  
    #                                  # Title can include an icon
    #                                  title = tagList(shiny::icon("gear"), "Spatial Pattern of Rj"),
    #                                  
    # 
    #                                  tabPanel("Point", 
    #                                           #status = "danger", collapsible = TRUE,
    #                                           plotOutput("Rj_pnt")),
    #                                  
    #                                  tabPanel("Hexagon", 
    #                                           sliderInput("Rj_hexagon_slider", "Number of bin:", 1, 100, 50),
    #                                           plotOutput("Rj_hexagon")) 
    #                               
    #                           )
    # 
    #                    )
    #                  )
    #           ),
    #           
    #           column(width = 8, 
    #                  fluidRow(
    #                    box(width = 24, 
    #                        height = 800, status = "danger", collapsible = TRUE,
    #                        
    #                        title = strong("Time-varying reproductive numbers"),
    #                        plotOutput("Rt_plot"),
    #                        plotOutput("Rj_spt_ptrn")
    #                        )
    # 
    #                  )
    # 
    #           )
    #         ) #fluidRow
    #   
    # ) #tabItem Rj
    
  ) #tabItems
  
)




dashboardPage(header, sidebar, body)

ui = dashboardPage(skin = "red", 
                   header, sidebar, body
                   )

#runApp("./DB_0106")



