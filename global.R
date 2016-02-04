# Install required packages if not exist
if (!require("shiny"))
  install.packages("shiny")
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
goevb = read.csv("alles.csv", stringsAsFactors = FALSE)
# Add artificial Trip ID
goevb$id = seq.int(nrow(goevb))

source('customFunctions.R')

# Aggregates all trips with the same origin latitude and longitude and count them.
locationTripsOrigin <-
  as.data.frame(table(goevb$origin_lon, goevb$origin_lat))
# Gives the prior created columns names to address them.
names(locationTripsOrigin) <- c('origin_lon', 'origin_lat', 'freq')
# TODO: Add destination latitude and longitude to locationTripsOrigin !!
# Add origin longitude to locationTripsOrigin.
locationTripsOrigin$origin_lon <-
  as.numeric(as.character(locationTripsOrigin$origin_lon))
# Add origin latitude to locationTripsOrigin.
locationTripsOrigin$origin_lat <-
  as.numeric(as.character(locationTripsOrigin$origin_lat))
# Filter out all Trips with freq = 0.
locationTripsOrigin <- subset(locationTripsOrigin, freq > 0)

# TODO: Extract as function
# Aggregates all trips with the same origin latitude and longitude and count them.
locationTripsDestination <-
  as.data.frame(table(goevb$destination_lon, goevb$destination_lat))
# Gives the prior created columns names to address them.
names(locationTripsDestination) <- c('destination_lon', 'destination_lat', 'freq')
# TODO: Add destination latitude and longitude to locationTripsDestination !!
# Add origin longitude to locationTripsDestination.
locationTripsDestination$destination_lon <-
  as.numeric(as.character(locationTripsDestination$destination_lon))
# Add origin latitude to locationTripsDestination.
locationTripsDestination$destination_lat <-
  as.numeric(as.character(locationTripsDestination$destination_lat))
# Filter out all Trips with freq = 0.
locationTripsDestination <- subset(locationTripsDestination, freq > 0)


locationTripsOrigin$freq_normalized <- NormalizeFreq(locationTripsOrigin$freq)
locationTripsOrigin$freq_a <- GenerateAlpha(locationTripsOrigin$freq_normalized)
locationTripsOrigin$freq_c <- GenerateColor(locationTripsOrigin$freq_normalized)
locationTripsOrigin$freq_r <- GenerateRadius(locationTripsOrigin$freq_normalized)

locationTripsDestination$freq_normalized <- NormalizeFreq(locationTripsDestination$freq)
locationTripsDestination$freq_a <- GenerateAlpha(locationTripsDestination$freq_normalized)
locationTripsDestination$freq_c <- GenerateColor(locationTripsDestination$freq_normalized)
locationTripsDestination$freq_r <- GenerateRadius(locationTripsDestination$freq_normalized)
