#user interface for Mall Data
shinyUI(dashboardPage(
  # Application title
  dashboardHeader(title = "Retail Analytics"),

  # Sidebar with a slider input for number of bins

    dashboardSidebar(
      h3("Door Traffic"),
      # Select Justices name here


    #   dateRangeInput('dateRange',
    #   label = 'Date range input:',
    #   start = as.Date("2017-07-01"), end = Sys.Date()
    #   ),
      checkboxGroupInput("weekdays_checked", "Weekdays:",

                         choices = c("Monday",
                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                           selected = c("Monday",
                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

        )
    ),

    # Show a plot of the generated distribution

    dashboardBody(
              useShinyjs(),
              tags$head(tags$style(HTML('.info-box-icon {width: 50%,} .info-box-content {padding-top: 10%; padding-bottom: 10%;}'))),
              fluidRow(
              column(6,uiOutput("dateChoices")),
              column(6,selectizeInput("door",
                          label = "Select Door/Store:",
                          choices = unique(mall_Data$Door),
                          multiple = T,
                          options = list(maxItems = 5,
                                         placeholder = 'Select Door/Store',
                                         'plugins' = list('remove_button'))

                          )
                          )),
            div(id="tabs",
                fluidRow(
                    # infoBoxOutput("totalCount", width = 2),
                    # infoBoxOutput("year2year", width = 2),
                    # infoBoxOutput("week2week", width = 2),
                    box(title = "Total Visitors", textOutput("totalCount_txt"), status = "info", width = 2),
                    #box(title = "YoY", textOutput("year2year_txt"), status = "info", width = 2)
                    uiOutput("year2year_txt"),
                    uiOutput("week2week_txt"),
                    infoBoxOutput("weekendPerc", width = 2),
                    infoBoxOutput("busiestDoor", width = 2)


                    )
            ),

                fluidRow(
                column(6,plotlyOutput("week2weekPlot")),
                column(6,plotlyOutput("heatMapPlot")))
    #           div(id = "tabs",
    #            tabsetPanel(
    #               tabPanel("Insights", infoBoxOutput("totalCount"),
    #                                    infoBoxOutput("weekendPerc"),
    #                                    infoBoxOutput("busiestHour"),
    #                                    infoBoxOutput("busiestDoor"),
    #                                    infoBoxOutput("busiestDay"),
    #                                    infoBoxOutput("week2week"),
    #                                    infoBoxOutput("year2year")),
    #               tabPanel("Day", plotlyOutput("dayPlot", height = 500)),
    #               tabPanel("Hour", plotlyOutput("hourPlot",  height = 500)),
    #               tabPanel("Weekday", plotlyOutput("weekdayPlot",  height = 500))
    #
    # )
    # )
    )
)
)
