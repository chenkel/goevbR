# Install required packages if not exist
if (!require("shiny"))
  install.packages("shiny")
if (!require("data.table"))
  install.packages("data.table")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("Cairo"))
  install.packages("Cairo")
if (!require("scales"))
  install.packages("scales")

if (!require("extrafont"))
  install.packages("extrafont")
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
library(ggplot2)
library(Cairo)
library(scales)
library(data.table)
library(extrafont)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# Import data

goevb = read.csv("alles.csv", sep=",", fileEncoding="UTF-8", stringsAsFactors = FALSE)



# ## Converting the date to a recognizable format
goevb$datetime <-
  strptime(goevb$datetime, format = '%d/%m/%Y:%H:%M:%S')

# ## Getting the day and hour of each trip
goevb$day <- goevb$datetime$wday
goevb$day_f <- factor(goevb$day, labels = c("So","Mo","Di","Mi","Do","Fr","Sa"))
goevb$hour <- goevb$datetime$hour
goevb$hour_f <- factor(goevb$hour)
tripFilter <- reactiveValues(
 hour = c(0:23),
 weekday = c(0:6)
)
keptTrips <- goevb
excludedTrips <- goevb[0, ]
keptTripsForStop <- goevb

tripsOrig <- NULL
tripsDest <- NULL

trips <- reactiveValues(
  keeprows = rep(TRUE, nrow(goevb))
)

shouldRedrawMapOrig <- FALSE

source('customFunctions.R')

AggregateAllTrips(keptTrips)
