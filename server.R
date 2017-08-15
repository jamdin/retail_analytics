#server script for United Nations Advanced Example
library(ggthemes)

shinyServer(function(input, output, session) {

  output$dayPlot <- renderPlotly({
    # mall_Data$Date <- as.POSIXct(mall_Data$Date)
    if (length(input$door)==0){ print("Please select at least one door")
    ggplot()}

    else {
      df_trend <- mall_Data  %>%
        filter(Door %in% input$door) %>%
        filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
        as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

      df_trend$Weekday = weekdays(df_trend$Date)
      df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
      "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

      df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)


      df_trend$Date = as.Date(df_trend$Date, tz = "PST8PDT")
      df_agg <- aggregate(Count~Date+Door, data = df_trend, sum)
      gg_doors <- ggplot(data = df_agg, aes(x = Date, y = Count, color = Door))+
      geom_line() + scale_x_date()

      gg_doors
    }
  })

output$hourPlot <- renderPlotly({
  # mall_Data$Date <- as.POSIXct(mall_Data$Date)
  if (length(input$door)==0){ print("Please select at least one door")
  ggplot()}

  else {
    df_trend_hour <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend_hour$Weekday = weekdays(df_trend_hour$Date)
    df_trend_hour$Weekday <- factor(df_trend_hour$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend_hour <- df_trend_hour %>% filter(Weekday %in% input$weekdays_checked)
    print(mall_Data$Date)
    df_trend_hour$Hour = as.POSIXct(strftime(df_trend_hour$Date, format="%H:%M", , tz = "GMT"),format="%H:%M", tz = "GMT")
    df_agg <- aggregate(Count~Hour+Door, data = df_trend_hour, median)
    gg_doors <- ggplot(data = df_agg, aes(x = Hour, y = Count, color = Door))+
    geom_line() + xlab("") + scale_x_datetime(date_breaks = "2 hour",
                       date_labels = "%I %p")

    gg_doors
  }
})


output$weekdayPlot <- renderPlotly({
  # mall_Data$Date <- as.POSIXct(mall_Data$Date)
  if (length(input$door)==0){ print("Please select at least one door")
  ggplot()}

  else {
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)

    #df_agg <- aggregate(Count~Weekday+Door, data = df_trend, mean)
    gg_doors <- ggplot(data = df_trend[df_trend$Count!=0,], aes(x = Count, color = Door))+
    geom_density() + xlab("") + theme(axis.text.x = element_text(angle = 0, hjust = 1))+
    facet_wrap(~Weekday)

    gg_doors
  }
})

output$totalCount <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)
    totalCount = sum(df_trend$Count)
    valueBox(value = totalCount,
             subtitle = paste("Total Visitors"),
             icon = icon("users"))
    }
})


output$weekendPerc <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)
    totalCount = sum(df_trend$Count)
    weekendCount = sum(df_trend$Count[df_trend$Weekday %in% c("Friday", "Saturday", "Sunday")])
    weekendPerc = paste0(round(100*weekendCount/totalCount,0),"%")
    valueBox(value = weekendPerc,
             subtitle = paste("of visitors come on the weekend"),
             icon = icon("hourglass-end"))
    }

})

output$busiestHour <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)
    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M", tz = "GMT"),format="%H:%M", tz = "GMT")
    df_agg <- aggregate(Count~Hour+Door, data = df_trend, mean)
    busiestHour <- df_agg[which.max(df_agg$Count), "Hour"]
    busiestHour <- strftime(busiestHour, format="%I:%M %p")
    valueBox(value = busiestHour,
             subtitle = paste("Busiest time of day"),
             icon = icon("clock-o"))
    }
})

output$busiestDoor <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)

    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M"),format="%H:%M")

    df_agg <- aggregate(Count~Door, data = df_trend, sum)
    busiestDoor <- df_agg[which.max(df_agg$Count), "Door"]
    valueBox(value = busiestDoor,
             subtitle = paste("is the door with the most traffic"),
             icon = icon("building"))
    }

})

output$busiestDay <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)

    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M"),format="%H:%M")
    df_trend$Date <- as.Date(df_trend$Date, tz = "PST8PDT")

    df_agg <- aggregate(Count~Date, data = df_trend, sum)
    busiestDate <- df_agg[which.max(df_agg$Count), "Date"]
    busiestDate <- as.Date(busiestDate)
    valueBox(value = busiestDate,
             subtitle = paste("was the busiest date in the range"),
             icon = icon("calendar"))
    }

})

output$week2week <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)

    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M"),format="%H:%M")


    df_trend$Week <- strftime(df_trend$Date, format = "%W", tz = "PST8PDT")
    df_trend$Weekday_number <- as.numeric(strftime(df_trend$Date, format = "%w", tz = "PST8PDT"))
    df_trend$Weekday_number <- (df_trend$Weekday_number+6)%%7 #Start on Monday

    last_day_weekday_number <- tail(df_trend$Weekday_number,1)
    df_trend_agg <- df_trend %>% filter(Weekday_number <= last_day_weekday_number)
    df_agg <- aggregate(Count~Week, data = df_trend_agg, sum)
    last_week <- max(df_agg$Week)
    last_week_count <- df_agg[df_agg$Week==as.numeric(last_week),"Count"]
    previous_week_count <- df_agg[df_agg$Week==(as.numeric(last_week)-1), "Count"]
    week2week <- round(100*(last_week_count/previous_week_count-1),2)

    if (week2week <0){ #Less number of guests
        valueBox(value = paste0("-",abs(week2week),"%"),
                 subtitle = paste("Decrease in week-to-week traffic"),
                 icon = icon("arrow-circle-down"),
                 color = "red")
    }else{ #More guests this week
        valueBox(value = paste0("+",abs(week2week),"%"),
                 subtitle = paste("Increase in week-to-week traffic"),
                 icon = icon("arrow-circle-up"),
                 color = "green")
    }
    }

})

output$year2year <- renderValueBox({
    if (length(input$door)==0){ print("Please select at least one door")
    }

    else{
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(input$dateRange[1], tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(input$dateRange[2], tz = "PST8PDT"))

    df_trend$Weekday = weekdays(df_trend$Date)
    df_trend$Weekday <- factor(df_trend$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    df_trend <- df_trend %>% filter(Weekday %in% input$weekdays_checked)

    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M"),format="%H:%M")

    last_year_count <- sum(df_trend$Count)

    minDateRange = strsplit(as.character(input$dateRange[1]), split = "-")[[1]]
    minDateRange[1] = as.character(as.numeric(minDateRange[1])-1)
    minDateRange_prevYear = paste0(minDateRange, collapse = "-")

    maxDateRange = strsplit(as.character(input$dateRange[2]), split = "-")[[1]]
    maxDateRange[1] = as.character(as.numeric(maxDateRange[1])-1)
    maxDateRange_prevYear = paste0(maxDateRange, collapse = "-")


    df_trend_prevYear <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      filter(as.Date(Date, tz = "PST8PDT") >= as.Date(minDateRange_prevYear, tz = "PST8PDT") &
      as.Date(Date, tz = "PST8PDT") <= as.Date(maxDateRange_prevYear, tz = "PST8PDT"))
    df_trend_prevYear$Weekday = weekdays(df_trend_prevYear$Date)
    df_trend_prevYear$Weekday <- factor(df_trend_prevYear$Weekday, levels= c("Monday",
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    df_trend_prevYear <- df_trend_prevYear %>% filter(Weekday %in% input$weekdays_checked)

    df_trend_prevYear$year <- strftime(df_trend_prevYear$Date, format = "%Y", tz = "PST8PDT")


    previous_year_count <- sum(df_trend_prevYear$Count)
    year2year <- round(100*(last_year_count/previous_year_count-1),2)

    if (year2year <0){ #Less number of guests
        valueBox(value = paste0("-",abs(year2year),"%"),
                 subtitle = paste("Decrease in year-to-year traffic"),
                 icon = icon("arrow-circle-down"),
                 color = "red")
    }else{ #More guests this week
        valueBox(value = paste0("+",abs(year2year),"%"),
                 subtitle = paste("Increase in year-to-year traffic"),
                 icon = icon("arrow-circle-up"),
                 color = "green")
    }
    }

})


})
