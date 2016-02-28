options(shiny.autoreload = FALSE, shiny.trace = FALSE, warn = 0)

# ## Install and load external packages
source('externalLibs.R')

# load preprocessed data from RData format
# (see importGoevbData.R for details on the import procedure)
goevb <- readRDS("goevb.rds")

# Define heatmap color palate
heatmapCols <- brewer.pal(8, "YlOrRd")

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

# ## Same as above (see Orig's comments)
tripFilterDest <- reactiveValues(hour = c(0:23),
                                 weekday = c(1:7))
shouldKeepDest <- reactiveValues(flag = rep(TRUE, nrow(goevb)))
tripsDest <- reactiveValues(kept = goevb,
                            excluded = goevb[0, ])
agTripsDest <- reactiveValues(kept = NULL,
                              excluded = NULL)
chosenStopDest <- reactiveValues(trips = goevb, name = ' ')

source('customFunctions.R')