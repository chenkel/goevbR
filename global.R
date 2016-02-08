# Install required packages if not exist
if (!require("shiny"))
  install.packages("shiny")
if (!require("data.table"))
  install.packages("data.table")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("shinydashboard"))
  install.packages("shinydashboard")
if (!require("leaflet"))
  install.packages("leaflet")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("RColorBrewer"))
  install.packages("RColorBrewer")

# External Libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# Import data
goevb = read.csv("alles.csv", sep=",", fileEncoding="UTF-8", stringsAsFactors = FALSE)

# ## Converting the date to a recognizable format
goevb$datetime <- strptime(goevb$datetime, format = '%d/%m/%Y:%H:%M:%S')

# ## Getting the day and hour of each trip
goevb$day <- goevb$datetime$wday
goevb$hour <- goevb$datetime$hour


goevbFilteredByHour <- goevb
# goevbFilteredByHour <- goevb[goevb$hour == 13, ]
# bla <- aggregate(hour ~ origin_lat + origin_lon, data = goevbFilteredByHour, FUN = length)


source('customFunctions.R')

# Aggregates all trips with the same origin latitude and longitude and count them.
locationTripsOrigin <- AggregateTrips(goevbFilteredByHour$origin_lat, goevbFilteredByHour$origin_lon, 0)
locationTripsOriginLength <- length(locationTripsOrigin[,1])
locationTripsDestination <- AggregateTrips(goevbFilteredByHour$destination_lat, goevbFilteredByHour$destination_lon, locationTripsOriginLength)