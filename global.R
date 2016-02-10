# ## Install and load external packages
source('externalLibs.R')

# ## Import data
goevb = read.csv(
  "goevb.csv",
  sep = ",",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# ## Converting the date to a recognizable format
goevb$datetime <-
  strptime(goevb$datetime, format = '%d/%m/%Y:%H:%M:%S')

# ## Get the day and hour of each trip
goevb$day <- goevb$datetime$wday
# start week by Monday(1) and end by Sunday(7)
goevb$day[goevb$day == 0] = 7
goevb$day_f <-
  factor(goevb$day, labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))
goevb$hour <- goevb$datetime$hour
goevb$hour_f <- factor(goevb$hour)


# ## Initially add all trips to keptTrips

# # ## ExcludedTrips initially empty
# excludedTrips <- goevb[0,]

# setup trip filters
tripFilterOrig <- reactiveValues(hour = c(0:23),
                                 weekday = c(1:7))
# boolean flag as a filter (non destructive)
shouldKeepOrig <- reactiveValues(flag = rep(TRUE, nrow(goevb)))
# filtered trips (with tripFilters, see tripsInBoundsOrig)
tripsOrig <- reactiveValues(kept = goevb,
                            excluded = goevb[0, ])
# aggregated trips for the heatmap
agTripsOrig <- reactiveValues(kept = NULL,
                              excluded = NULL)
# chosen bus stop (used for detailModal)
chosenStopOrig <- reactiveValues(trips = goevb, name = ' ')
# shouldRedrawMapOrig <- TRUE

# ## Same as above (see Orig's comments)
tripFilterDest <- reactiveValues(hour = c(0:23),
                                 weekday = c(1:7))
shouldKeepDest <- reactiveValues(flag = rep(TRUE, nrow(goevb)))
tripsDest <- reactiveValues(kept = goevb,
                            excluded = goevb[0, ])
agTripsDest <- reactiveValues(kept = NULL,
                              excluded = NULL)
chosenStopDest <- reactiveValues(trips = goevb, name = ' ')
# shouldRedrawMapDest <- TRUE

source('customFunctions.R')
