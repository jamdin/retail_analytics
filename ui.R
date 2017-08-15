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
                    infoBoxOutput("totalCount", width = 3),
                    infoBoxOutput("year2year", width = 3),
                    infoBoxOutput("week2week", width = 3),
                    infoBoxOutput("weekendPerc", width = 3),
                    infoBoxOutput("busiestDoor", width = 3)


                )
                ),
                fluidRow(
                column(6,plotlyOutput("week2weekPlot")))
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
