# ## Install and load external packages
source('helper/externalLibs.R')

# ## Include various helper functions
source('helper/customFunctions.R')

# ## load preprocessed data from RData format
# ## see data/importGoevbData.R for details on the import procedure
goevb <- readRDS("data/goevb.rds")

# ## Define heatmap color palate
heatmapCols <- brewer.pal(8, "YlOrRd")

# ## setup trip filters
tripFilterOrig <- reactiveValues(hour = c(0:23),
                                 weekday = c(1:7))
tripFilterDest <- reactiveValues(hour = c(0:23),
                                 weekday = c(1:7))

# ## boolean flag as a filter (non destructive)
shouldKeepOrig <- reactiveValues(flag = rep(TRUE, nrow(goevb)))
shouldKeepDest <- reactiveValues(flag = rep(TRUE, nrow(goevb)))

# ## filtered trips (with tripFilters, see tripsInBoundsOrig)
# ## initially add all trips to keptTrips and set excluded to zero elements
tripsOrig <- reactiveValues(kept = goevb,
                            excluded = goevb[0, ])
tripsDest <- reactiveValues(kept = goevb,
                            excluded = goevb[0, ])

# ## aggregated trips for the heatmap
agTripsOrig <- reactiveValues(kept = NULL,
                              excluded = NULL)
agTripsDest <- reactiveValues(kept = NULL,
                              excluded = NULL)

# ## chosen bus stop (used for detailModal)
chosenStopOrig <- reactiveValues(trips = goevb, name = ' ')
chosenStopDest <- reactiveValues(trips = goevb, name = ' ')
