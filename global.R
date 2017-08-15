# Golbal environment for both ui and server
# Include lines 3-6 in all global.R scripts
library(plotly)
library(shiny)
library(dplyr)
library(shinydashboard)

source("plotlyGraphWidget.R")

mall_Data <- read.csv("Data/door_data.csv", stringsAsFactors = FALSE)

mall_Data$Date <- as.POSIXct(mall_Data$Date, tz = "GMT")
class(mall_Data$Date) = c('POSIXt','POSIXct')
