#server script for United Nations Advanced Example
library(ggthemes)

shinyServer(function(input, output, session) {

  output$dayPlot <- renderPlotly({
    # mall_Data$Date <- as.POSIXct(mall_Data$Date)
    if (length(input$door)==0){ print("Please select at least one door")
    ggplot()}

    else {
      df_trend <- mall_Data  %>%
        filter(Door %in% input$door)

      # Graph title
      if (length(input$door)>2) {
        j_doors_comma <- paste(input$door[-length(input$door)], collapse = ', ')
        j_doors <- paste(j_doors_comma, ", and ", input$door[length(input$door)],
                         sep="")
      }

      else{
        j_doors <- paste(input$door, collapse = ' and ')
      }

      graph_title  <- paste("Count Data for ", j_doors, sep="")
      df_trend$Date = as.Date(df_trend$Date)
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
    df_trend <- mall_Data  %>%
      filter(Door %in% input$door)

    # Graph title
    if (length(input$door)>2) {
      j_doors_comma <- paste(input$door[-length(input$door)], collapse = ', ')
      j_doors <- paste(j_doors_comma, ", and ", input$door[length(input$door)],
                       sep="")
    }

    else{
      j_doors <- paste(input$door, collapse = ' and ')
    }

    graph_title  <- paste("Count Data for ", j_doors, sep="")
    df_trend$Hour = as.POSIXct(strftime(df_trend$Date, format="%H:%M"),format="%H:%M")
    df_agg <- aggregate(Count~Hour+Door, data = df_trend, mean)
    gg_doors <- ggplot(data = df_agg, aes(x = Hour, y = Count, color = Door))+
    geom_line() + xlab("Time") + scale_x_datetime(date_breaks = "1 hour",
                       date_labels = "%I:%M %p") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

    gg_doors
  }
})


  output$termPlot <- renderPlot({
    df_term <- mall_Data  %>%
      filter(Door %in% input$door) %>%
      group_by(Door) %>%
      summarise(terms = median(Count))

    trans_theme <- theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill=NA),
      plot.background = element_rect(fill=NA)
    )

    ggplot(df_term, aes(x=reorder(Door, terms), y=terms))+
      geom_bar(stat = "identity", fill = "#2980b9")+
      theme_bw()+
      trans_theme+
      labs(y="Median Count (30 minute interval)", x="")+
      coord_flip()
  }, bg="transparent")
})
