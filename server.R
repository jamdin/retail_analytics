#server script for United Nations Advanced Example
library(ggthemes)

shinyServer(function(input, output, session) {


  output$dateChoices <- renderUI({
    selectInput("dateChoices",
                label = "Select Year-Week:",
                c(as.list(dateChoices_array)))
  })

  current_week <- reactive({
      dateChoices <- input$dateChoices
      weekYear <- strsplit(dateChoices, " ")[[1]][1]

      cw <- mall_Data  %>% filter(Door %in% input$door) %>%
      filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")==weekYear) %>%
      filter(Weekday %in% input$weekdays_checked)
      cw
      })

observe({
        #Hide insights panel until there are doors and dates chosen
        condition = length(input$door)!=0 && input$dateChoices!=""
        toggle(id = "tabs", condition = condition)
      })


output$totalCount <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    totalCount = sum(current_week()$Count)
    infoBox(value = totalCount,
             title = paste("Total Visitors"),
             icon = icon("users"), width = 2)
    }
})


output$weekendPerc <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    totalCount = sum(current_week()$Count)
    weekendCount = sum(current_week()$Count[current_week()$Weekday %in% c("Friday", "Saturday", "Sunday")])
    weekendPerc = paste0(round(100*weekendCount/totalCount,0),"%")
    infoBox(value = weekendPerc,
             title = paste("of visitors come on the weekend"),
             icon = icon("hourglass-end"), width = 2)
    }

})

output$busiestHour <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    df_agg <- aggregate(Count~Hour+Door, data = current_week(), median)
    busiestHour <- df_agg[which.max(df_agg$Count), "Hour"]
    busiestHour <- strftime(busiestHour, format="%I:%M %p")
    infoBox(value = busiestHour,
             title = paste("Busiest time of day"),
             icon = icon("clock-o"), width = 2)
    }
})

output$busiestDoor <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    df_agg <- aggregate(Count~Door, data = current_week(), sum)
    busiestDoor <- df_agg[which.max(df_agg$Count), "Door"]
    infoBox(value = busiestDoor,
             title = paste("is the door with the most traffic"),
             icon = icon("building"), width = 2)
    }

})

output$busiestDay <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    df_agg <- aggregate(Count~Date, data = current_week(), sum)
    busiestDate <- df_agg[which.max(df_agg$Count), "Date"]
    busiestDate <- as.Date(busiestDate)
    infoBox(value = busiestDate,
             title = paste("was the busiest date in the range"),
             icon = icon("calendar"), width = 2)
    }

})

output$week2week <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{
    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]
    year = strsplit(weekYear,"-")[[1]][1]
    week = strsplit(weekYear, "-")[[1]][2]

    minWeek = paste0(year, "-", as.character(as.numeric(week)-1))
    maxWeek = weekYear

    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)

    df_agg <- aggregate(Count~Week, data = df_trend_agg, sum)
    last_week <- max(df_agg$Week)
    last_week_count <- df_agg[df_agg$Week==as.numeric(last_week),"Count"]
    previous_week_count <- df_agg[df_agg$Week==(as.numeric(last_week)-1), "Count"]
    week2week <- round(100*(last_week_count/previous_week_count-1),2)

    if (week2week <0){ #Less number of guests
        infoBox(value = paste0("-",abs(week2week),"%"),
                 title = paste("WoW"),
                 icon = icon("arrow-circle-down"),
                 color = "red", width = 2)
    }else{ #More guests this week
        infoBox(value = paste0("+",abs(week2week),"%"),
                 title = paste("WoW"),
                 icon = icon("arrow-circle-up"),
                 color = "green", width = 2)
    }
    }

})

output$year2year <- renderInfoBox({
    if (length(input$door)==0 || input$dateChoices==""){ infoBox(value = "", title = "")
    }

    else{

    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]
    year = strsplit(weekYear,"-")[[1]][1]
    week = strsplit(weekYear, "-")[[1]][2]
    minWeek_currentYear = paste0(year, "-01")
    maxWeek_currentYear = weekYear
    maxWeek_prevYear = paste0(as.character(as.numeric(year)-1), "-", week)
    minWeek_prevYear = paste0(as.character(as.numeric(year)-1), "-01")

    current_week_allYear <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek_currentYear &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek_currentYear) %>%
    filter(Weekday %in% input$weekdays_checked)

      current_week_prevYear <- mall_Data  %>% filter(Door %in% input$door) %>%
      filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek_prevYear &
             strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek_prevYear) %>%
      filter(Weekday %in% input$weekdays_checked)


    last_year_count <- sum(current_week_allYear$Count)
    previous_year_count <- sum(current_week_prevYear$Count)

    year2year <- round(100*(last_year_count/previous_year_count-1),2)

    if (year2year <0){ #Less number of guests
        infoBox(value = paste0("-",abs(year2year),"%"),
                 title = paste("YoY"),
                 icon = icon("arrow-circle-down"),
                 color = "red", width = 2)
    }else{ #More guests this week
        infoBox(value = paste0("+",abs(year2year),"%"),
                 title = paste("YoY"),
                 icon = icon("arrow-circle-up"),
                 color = "green", width = 2)
    }
    }

})

output$week2weekPlot <- renderPlotly({
    if (length(input$door)==0 || input$dateChoices==""){ ggplot()
    }

    else{
    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]
    year = strsplit(weekYear,"-")[[1]][1]
    week = strsplit(weekYear, "-")[[1]][2]

    minWeek = paste0(year, "-", as.character(as.numeric(week)-4))
    maxWeek = weekYear

    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)

    df_agg <- aggregate(Count~Week+Door, data = df_trend_agg, sum)

    gg_week <- ggplot(data = df_agg, aes(x = Week, y = Count, fill = Door))+
    geom_col(position = "dodge") + theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.background = element_blank())

    gg_week
    }

})





})
