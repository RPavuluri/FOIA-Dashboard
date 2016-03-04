# FOIA MINER
# ui.R
# Chicago Department of Innovation and Technology
#
# Dashboard to track 1) most popular words in FOIA requests
# from selected time period, 2) how the most popular words change
# each month, 3) the volume of FOIA requests by month, and 4) words that
# demonstrate a statistially significant rise in volume when compared to
# their average frequency in previous six months.
# Dashboard built for several City of Chicago Departments.

################################################################################

shinyUI(fluidPage(
  h1("City of Chicago FOIA Miner", align = "center"),
  h3("Department of Innovation and Technology", align = "center"),
  br(),

  #sidebar and sidepanel for dep't options and download buttons
  sidebarLayout(
    sidebarPanel(helpText("Use these options to find trends in
                          Freedom of Information Act requests."),
      selectInput("department",
          label = "Select Department",
          choices = list("Innovation and Technology", "License Appeal Commission",
                        "Family & Support Services", "Finance", "Fleet & Facility Management",
                        "Planning & Development", "Water Management", "Buildings",
                        "Streets & Sanitation", "Transportation", "Mayor's Office",
                        "Animal Care & Control")),
    dateRangeInput("date", label = "Select Date Range", start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
    br(),
    br(),
    br(),
    br(),
    br(),
    downloadButton('overall_data', 'Download Top 20 Words Overall'),
    br(),
    br(),
    downloadButton('month_data', 'Download Top 5 Words by Month'),
    br(),
    br(),
    downloadButton('volume_data', 'Download Requests by Month'),
    br(),
    br(),
    downloadButton('rising_data', 'Download Rising Words by Month')
    ),

    #main panel to display plots and tables
    mainPanel(
      tabsetPanel(
        tabPanel("Top Words Overall", plotOutput("overall_plot")),
        tabPanel("Top Words By Month", plotOutput("month_plot")),
        tabPanel("Request Volume", br(), textOutput("rec_req"), textOutput("volume_text"), plotOutput("volume_plot", click = "plot_click")),
        tabPanel("Rising Words", br(), textOutput("rising_explanation"), plotOutput("rising_plot"), tableOutput("rising_table"))
      )
    )
  )
))