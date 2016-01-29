# Install required packages if not exist
if (!require("shiny"))
  install.packages("shiny")
if (!require("shinydashboard"))
  install.packages("shinydashboard")
if (!require("leaflet"))
  install.packages("leaflet")
if (!require("dplyr"))
  install.packages("dplyr")

# External Libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)

# Import data
goevb = read.csv("alles.csv", stringsAsFactors = FALSE)
# Add artificial Trip ID
goevb$id = seq.int(nrow(goevb))

source('customFunctions.R')

# Aggregates all trips with the same origin latitude and longitude and count them.
locationTrips <-
  as.data.frame(table(goevb$origin_lon, goevb$origin_lat))
# Gives the prior created columns names to address them.
names(locationTrips) <- c('origin_lon', 'origin_lat', 'freq')
# TODO: Add destination latitude and longitude to locationTrips !!
# Add origin longitude to locationTrips.
locationTrips$origin_lon <-
  as.numeric(as.character(locationTrips$origin_lon))
# Add origin latitude to locationTrips.
locationTrips$origin_lat <-
  as.numeric(as.character(locationTrips$origin_lat))
# Filter out all Trips with freq = 0.
locationTrips <- subset(locationTrips, freq > 0)

locationTrips$freq_normalized <- NormalizeFreq(locationTrips$freq)
locationTrips$freq_a <- GenerateAlpha(locationTrips$freq_normalized)
locationTrips$freq_c <- GenerateColor(locationTrips$freq_normalized)
