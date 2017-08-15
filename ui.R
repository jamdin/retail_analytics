#user interface for Mall Data
shinyUI(dashboardPage(

  # Application title
  dashboardHeader(title = "Retail Analytics"),

  # Sidebar with a slider input for number of bins
 
    dashboardSidebar(
      h3("Door Traffic"),
      # Select Justices name here
      selectizeInput("door",
                  label = "Door(s) of Interest",
                  choices = unique(mall_Data$Door),
                  multiple = T,
                  options = list(maxItems = 5,
                                 placeholder = 'Select a door',
                                 'plugins' = list('remove_button')),
                  selected = "DoorA"

                  ),


      dateRangeInput('dateRange',
      label = 'Date range input:',
      start = as.Date("2017-07-01"), end = Sys.Date()
      ),
      checkboxGroupInput("weekdays_checked", "Weekdays:",

                         choices = c("Monday",
                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                           selected = c("Monday",
                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

        )
    ),

    # Show a plot of the generated distribution

    dashboardBody(
               tabsetPanel(
                  tabPanel("Insights", valueBoxOutput("totalCount"),
                                       valueBoxOutput("weekendPerc"),
                                       valueBoxOutput("busiestHour"),
                                       valueBoxOutput("busiestDoor"),
                                       valueBoxOutput("busiestDay"),
                                       valueBoxOutput("week2week"),
                                       valueBoxOutput("year2year")),
                  tabPanel("Day", plotlyOutput("dayPlot", height = 500)),
                  tabPanel("Hour", plotlyOutput("hourPlot",  height = 500)),
                  tabPanel("Weekday", plotlyOutput("weekdayPlot",  height = 500))

    )
    )

)
)
