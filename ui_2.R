#user interface for Mall Data
shinyUI(fluidPage(

  # Application title
  titlePanel("Retail Analytics"),

  # Sidebar with a slider input for number of bins

    sidebarPanel(
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


      # Term plot
      plotOutput("termPlot", height = 200),

      dateRangeInput('dateRange',
      label = 'Date range input: yyyy-mm-dd',
      start = Sys.Date() - 7, end = Sys.Date()
      ),
      checkboxGroupInput("weekdays_checked", "Weekdays:",

                         choices = c("Monday",
                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                           selected = c("Monday",
                             "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

        ),


      helpText("Data from Mall collected in July 2017")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("dayPlot"),
      plotlyOutput("hourPlot"),
      plotlyOutput("weekdayPlot")
    )
  )
)
