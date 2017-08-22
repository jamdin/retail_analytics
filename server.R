library(ggthemes)
library(scales)
library(grid)


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

sales_week <- reactive({
    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]

    sw <- sales_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")==weekYear) %>%
    filter(Weekday %in% input$weekdays_checked)
    sw
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

output$totalCount_txt <- renderText({
    if (length(input$door)==0 || input$dateChoices==""){ "0"
    }

    else{
    totalCount = sum(current_week()$Count)
    prettyNum(totalCount, big.mark=",")
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
    prev_week = as.numeric(week)-1
    year = ifelse(prev_week<0, as.character(as.numeric(year)-1), year)
    prev_week = ifelse(prev_week<0,prev_week+53, prev_week)
    minWeek = paste0(year, "-", ifelse(prev_week<10,paste0("0",
                                as.character(prev_week)),as.character(prev_week))) #Add trailing 0
    maxWeek = weekYear
    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)
    df_agg <- aggregate(Count~Week, data = df_trend_agg, sum)
    last_week_count <- df_agg[2,2]
    previous_week_count <- df_agg[1,2]
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

week2week <- reactive({
    if (length(input$door)==0 || input$dateChoices==""){ 0
    }

    else{
    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]
    year = strsplit(weekYear,"-")[[1]][1]
    week = strsplit(weekYear, "-")[[1]][2]
    prev_week = as.numeric(week)-1
    year = ifelse(prev_week<0, as.character(as.numeric(year)-1), year)
    prev_week = ifelse(prev_week<0,prev_week+53, prev_week)
    minWeek = paste0(year, "-", ifelse(prev_week<10,paste0("0",
                                as.character(prev_week)),as.character(prev_week))) #Add trailing 0
    maxWeek = weekYear
    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)
    df_agg <- aggregate(Count~Week, data = df_trend_agg, sum)
    last_week_count <- df_agg[2,2]
    previous_week_count <- df_agg[1,2]
    round(100*(last_week_count/previous_week_count-1),2)
    }

})

output$week2week_txt <- renderUI({

    if (as.numeric(week2week()) <0){ #Less number of guests
        w2w_txt = paste0("-",abs(week2week()),"% ", "\u25BC")
        box(title = "WoW", w2w_txt, status = "danger", width = 2)
    }else{ #More guests this week
        w2w_txt = paste0("+",abs(week2week()),"% ", "\u25B2")
        box(title = "WoW", w2w_txt, status = "success", width = 2)
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
                 title = tags$p("YoY", style = "font-size: 50%;"),
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

year2year <- reactive({
        if (length(input$door)==0 || input$dateChoices==""){ 0
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

        round(100*(last_year_count/previous_year_count-1),2)
        }
})

output$year2year_txt <- renderUI({

    if (as.numeric(year2year()) <0){ #Less number of guests
        y2y_txt = paste0("-",abs(year2year()),"% ", "\u25BC")
        box(title = "YoY", y2y_txt, status = "danger", width = 2)
    }else{ #More guests this week
        y2y_txt = paste0("+",abs(year2year()),"% ", "\u25B2")
        box(title = "YoY", y2y_txt, status = "success", width = 2)
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
    prev_week = as.numeric(week)-4
    year = ifelse(prev_week<0, as.character(as.numeric(year)-1), year)
    prev_week = ifelse(prev_week<0,prev_week+53, prev_week)
    minWeek = paste0(year, "-", ifelse(prev_week<10,paste0("0",
                                as.character(prev_week)),as.character(prev_week))) #Add trailing 0

    maxWeek = weekYear

    #Sales
    df_sales_agg <- sales_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)
    df_sales_agg$yearWeek <- strftime(df_sales_agg$Date, format = "%Y-%W", tz = "PST8PDT")
    df_agg_sales <- aggregate(Sales~yearWeek+Door, data = df_sales_agg, sum)

    #Count
    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)
    df_trend_agg$yearWeek <- strftime(df_trend_agg$Date, format = "%Y-%W", tz = "PST8PDT")
    df_agg_count <- aggregate(Count~yearWeek+Door, data = df_trend_agg, sum)

    df_agg <- merge(df_agg_sales, df_agg_count)
    gg_week <- ggplot(data = df_agg, aes(x = yearWeek, fill = Door))+
    geom_col(aes(y = Count), position = "dodge") + theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.background = element_blank()) + xlab("Week")+ggtitle("Weekly View")

    # gg_week <- gg_week + geom_path(aes(y = Sales, group = Door, color = Door)) +
    #         scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Sales [$]"))

    gg_week
    }

})


output$heatMapPlot <- renderPlotly({
    if (length(input$door)==0 || input$dateChoices==""){ ggplot()
    }

    else{
    dateChoices <- input$dateChoices
    weekYear <- strsplit(dateChoices, " ")[[1]][1]
    year = strsplit(weekYear,"-")[[1]][1]
    week = strsplit(weekYear, "-")[[1]][2]
    prev_week = as.numeric(week)-14
    year = ifelse(prev_week<0, as.character(as.numeric(year)-1), year)
    prev_week = ifelse(prev_week<0,prev_week+53, prev_week)
    minWeek = paste0(year, "-", ifelse(prev_week<10,paste0("0",
                                as.character(prev_week)),as.character(prev_week))) #Add trailing 0

    maxWeek = weekYear

    df_trend_agg <- mall_Data  %>% filter(Door %in% input$door) %>%
    filter(strftime(Date, format = "%Y-%W", tz = "PST8PDT")<=maxWeek &
         strftime(Date, format = "%Y-%W", tz = "PST8PDT")>=minWeek) %>%
    filter(Weekday %in% input$weekdays_checked)
    df_trend_agg$yearWeek <- strftime(df_trend_agg$Date, format = "%Y-%W", tz = "PST8PDT")
    df_agg <- aggregate(Count~yearWeek+Week+Weekday, data = df_trend_agg, sum)
    df_agg$Week <- as.numeric(df_agg$Week)

    difference = apply(X = df_agg, 1, function(x) (round(100*((as.numeric(x[4])/df_agg[((as.numeric(x[2])-1)==df_agg$Week)&
          x[3]==df_agg$Weekday,4])-1),2))[1])
    df_agg <- cbind(df_agg, difference)
    df_agg <- df_agg[df_agg$Week!=prev_week,]
    df_agg$diffPerc <- paste0(as.character(df_agg$difference), "%")
    #df_agg <- df_agg[!is.na(df_agg),]

    gg_week <- ggplot(data = df_agg, aes(x = Weekday, y = yearWeek))+
    geom_tile(aes(fill = difference)) + geom_text(aes(label = diffPerc), size = 3) +
    theme(legend.position="none", panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.background = element_blank(),
    text = element_text(size=10))+
    scale_fill_gradient2(low = "firebrick", mid = "white", high = "forestgreen",
    limits = c(10,-10), oob = squish) + xlab(" ") + ylab(" ") + ggtitle("Daily View")

    gg_week
    }

})


### Sales Data

output$totalSales_txt <- renderText({
    if (length(input$door)==0 || input$dateChoices==""){ "0"
    }

    else{
    totalCount = sum(sales_week()$Sales)
    paste("$",prettyNum(totalCount, big.mark=","))
    }
})

output$RPV_txt <- renderText({
    if (length(input$door)==0 || input$dateChoices==""){ "0"
    }

    else{
    total_visitors = sum(current_week()$Count)
    total_sales = sum(sales_week()$Sales)
    RPV = round(total_sales/total_visitors,2)
    paste("$",prettyNum(RPV, big.mark=","))
    }
})




})
