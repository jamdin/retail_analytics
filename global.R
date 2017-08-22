# Golbal environment for both ui and server
# Include lines 3-6 in all global.R scripts
library(plotly)
library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyjs)

source("plotlyGraphWidget.R")

# Count Data

mall_Data <- read.csv("Data/door_data.csv", stringsAsFactors = FALSE)

mall_Data$Date <- as.POSIXct(mall_Data$Date, tz = "GMT")
class(mall_Data$Date) = c('POSIXt','POSIXct')

today = Sys.Date()
mall_Data = mall_Data[as.Date(mall_Data$Date, tz = "PST8PDT")<=today,]

mall_Data$week <- strftime(mall_Data$Date, format = "%Y-%W", tz = "PST8PDT")
mall_Data$date <- strftime(mall_Data$Date, format = "%m/%d", tz = "PST8PDT")
max_values <- aggregate(date~week, data = mall_Data, max)
min_values <- aggregate(date~week, data = mall_Data, min)
dates_minMax = cbind(min_values[,1], min_values[,2], max_values[,2])
dateChoices_array <- apply(dates_minMax, 1, function(x) paste0(x[1], " (", x[2], "-", x[3], ")"))
dateChoices_array <- dateChoices_array[order(dateChoices_array, decreasing = T)]

mall_Data$Weekday = weekdays(mall_Data$Date)
mall_Data$Weekday <- factor(mall_Data$Weekday, levels= c("Monday",
"Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

mall_Data$Hour = as.POSIXct(strftime(mall_Data$Date, format="%H:%M", tz = "GMT"),format="%H:%M", tz = "GMT")

mall_Data$Week <- strftime(mall_Data$Date, format = "%W", tz = "PST8PDT")
mall_Data$Weekday_number <- as.numeric(strftime(mall_Data$Date, format = "%w", tz = "PST8PDT"))
mall_Data$Weekday_number <- (mall_Data$Weekday_number+6)%%7 #Start on Monday



# Sales Data

sales_Data <- read.csv("Data/sales_data.csv", stringsAsFactors = FALSE)

sales_Data$Date <- as.POSIXct(sales_Data$Date, tz = "GMT")
class(sales_Data$Date) = c('POSIXt','POSIXct')

today = Sys.Date()
sales_Data = sales_Data[as.Date(sales_Data$Date, tz = "PST8PDT")<=today,]

sales_Data$week <- strftime(sales_Data$Date, format = "%Y-%W", tz = "PST8PDT")
sales_Data$date <- strftime(sales_Data$Date, format = "%m/%d", tz = "PST8PDT")
max_values <- aggregate(date~week, data = sales_Data, max)
min_values <- aggregate(date~week, data = sales_Data, min)
dates_minMax = cbind(min_values[,1], min_values[,2], max_values[,2])
dateChoices_array <- apply(dates_minMax, 1, function(x) paste0(x[1], " (", x[2], "-", x[3], ")"))
dateChoices_array <- dateChoices_array[order(dateChoices_array, decreasing = T)]

sales_Data$Weekday = weekdays(sales_Data$Date)
sales_Data$Weekday <- factor(sales_Data$Weekday, levels= c("Monday",
"Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

sales_Data$Hour = as.POSIXct(strftime(sales_Data$Date, format="%H:%M", tz = "GMT"),format="%H:%M", tz = "GMT")

sales_Data$Week <- strftime(sales_Data$Date, format = "%W", tz = "PST8PDT")
sales_Data$Weekday_number <- as.numeric(strftime(sales_Data$Date, format = "%w", tz = "PST8PDT"))
sales_Data$Weekday_number <- (sales_Data$Weekday_number+6)%%7 #Start on Monday
