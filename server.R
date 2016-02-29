function(input, output, session) {
  # # Controllers for origin and destination tabs
  source('controllers/origController.R', local = TRUE)
  source('controllers/destController.R', local = TRUE)
  
  
  # #  Functions used by both controllers # #
  # ## Draws heatmap circels in map
  RedrawMap <- function(mapId) {
    if (mapId == 'mapOrig') {
      theData <- agTripsOrig
    } else {
      theData <- agTripsDest
    }
    qpal <- colorBin("YlOrRd", theData$kept$freq, bins = 5)
    
    # Draw only kept data, since no excluded datapoints exist
    if (is.null(theData$excluded) && !is.null(theData$kept)) {
      return(
        leafletProxy(mapId, data = theData) %>%
          clearMarkers() %>%
          addCircleMarkers(
            theData$kept$longitude,
            theData$kept$latitude,
            radius = theData$kept$freq_r,
            color = theData$kept$freq_c,
            fillOpacity = theData$kept$freq_a,
            layerId = theData$kept$id,
            stroke = FALSE
          ) %>%
          addLegend(
            pal = qpal,
            values = theData$kept$freq,
            opacity = 0.8,
            title = "Häufigkeit",
            layerId = 'theLegend'
          )
      )
      
    } # Draw kept and excluded data
    else if (!is.null(theData$excluded) &&
             !is.null(theData$kept)) {
      return(
        leafletProxy(mapId, data = theData) %>%
          clearMarkers() %>%
          addCircleMarkers(
            theData$excluded$longitude,
            theData$excluded$latitude,
            radius = theData$excluded$freq_r,
            color = "gray",
            fillOpacity = 0.2,
            layerId = theData$excluded$id,
            stroke = FALSE
          ) %>%
          addCircleMarkers(
            theData$kept$longitude,
            theData$kept$latitude,
            radius = theData$kept$freq_r,
            color = theData$kept$freq_c,
            fillOpacity = theData$kept$freq_a,
            layerId = theData$kept$id,
            stroke = FALSE
          ) %>%
          addLegend(
            pal = qpal,
            values = theData$kept$freq,
            opacity = 0.7,
            title = "Häufigkeit",
            layerId = 'theLegend'
          )
      )
    }
  }
  
  # ## Show a popup for the chosen location with basic information
  showTripPopup <- function(id, lat, lng, mapId) {
    if (mapId == 'mapOrig') {
      chosenTrip <- agTripsOrig$kept[agTripsOrig$kept$id == id,]
      stopName <- GetStopName(lat, lng, 0)
      modalId <- 'origModal'
    } else {
      chosenTrip <-
        agTripsDest$kept[agTripsDest$kept$id == id,]
      stopName <- GetStopName(lat, lng, 1)
      modalId <- 'destModal'
    }
    # Construct HTML content
    content <- as.character(
      tagList(
        tags$h4(stopName),
        sprintf('Häufigkeit: %s', chosenTrip$freq),
        br(),
        br(),
        sprintf('Breite: %s', round(chosenTrip$latitude, 4)),
        br(),
        sprintf('Länge: %s' , round(chosenTrip$longitude, 4)),
        br(),
        br(),
        # Include custom button to show modal
        modalButton(modalId,
                    label = "Details...",
                    icon = icon("server"))
      )
    )
    # Add popup to map and show it to the user
    leafletProxy(mapId) %>% addPopups(
      lng = chosenTrip$longitude,
      lat = chosenTrip$latitude,
      popup = content,
      layerId = chosenTrip$id,
      options = popupOptions(closeOnClick = TRUE)
    )
  }
  
  #' GetStopName
  #'
  #' Retrieves the stop name from given latitude and longitude from the goevb dataset.
  #'
  #' @param lat       The latitude information of the stop you are looking for.
  #' @param lng       The longitude information of the stop you are looking for.
  #' @param orig0dest1   Specifies whether to look for origin or destination stops in dataset.
  #'
  #' @return Subset of goevb containing stops with given lat and lng.
  #' @export
  #'
  #' @example
  #' GetStopName(54.12, 9.42, 0)
  #'
  GetStopName <- function(lat, lng, orig0dest1) {
    if (orig0dest1 == 0) {
      return(subset(goevb, origin_lat == lat & origin_lon == lng)[, 1][1])
    } else if (orig0dest1 == 1) {
      return(subset(goevb, destination_lat == lat &
                      destination_lon == lng)[, 2][1])
    }
  }
  
  #' AggregateTrips
  #'
  #' Groups all trips by location and counts the occurrences in these groups
  #'
  #' @param lat       The dataset containing latitude information of all trips.
  #' @param lng       The dataset containing longitude information of all trips.
  #' @param startId   Specifies the unique ID for the first aggregated trip. Used for Leaflet Popups later.
  #'
  #' @return Aggregated Trips with the following attributes
  #' \code{id} - identifies aggregated trip uniquely,
  #' \code{latitude}, \code{longitude} - unique for trip,
  #' \code{freq} - number of trips with the same latitude and longitude,
  #' \code{freq_normalized} - normalized frequency to avoid over-/underemphasizing
  #' \code{freq_a}, \code{freq_c}, \code{freq_r} - alpha, color and radius of aggregated trip for displaying the heatmap
  #' @export
  #'
  #' @example
  #' AggregateTrips(tripsOrig$keptin_lat, tripsOrig$keptin_lon, 0)
  #'
  AggregateTrips <- function(lat, lng, startId) {
    # cat(file = stderr()cat(file = stderr(), '4 AggregateTrips called',  '\n')
    if (is.null(lat)  || length(lat) == 0) {
      return(goevb[FALSE,])
    }
    
    locationTrips <- as.data.frame(table(lat, lng))
    # Gives the prior created columns names to address them.
    names(locationTrips) <- c('latitude', 'longitude', 'freq')
    # Add origin longitude to locationTripsOrigin.
    locationTrips$longitude <-
      as.numeric(as.character(locationTrips$longitude))
    # Add origin latitude to locationTripsOrigin.
    locationTrips$latitude <-
      as.numeric(as.character(locationTrips$latitude))
    # Filter out all Trips with freq = 0.
    locationTrips <- (subset(locationTrips, freq > 0))
    
    locationTrips$freq_normalized <-
      NormalizeFreq(locationTrips$freq)
    locationTrips$freq_a <-
      GenerateAlpha(locationTrips$freq_normalized)
    locationTrips$freq_c <-
      GenerateColor(locationTrips$freq_normalized)
    locationTrips$freq_r <-
      GenerateRadius(locationTrips$freq_normalized)
    
    locationTrips$id <- seq.int(nrow(locationTrips)) + startId
    locationTrips$id
    
    return(locationTrips)
  }
  
  #' AggregateAllTrips
  #'
  #' Aggregates by origin and destination
  #'
  #' @param tripsOrig   Specifies all kept and excluded origin trips.
  #' @param tripsDest   Specifies all kept and excluded destination trips.
  #'
  #' @return Global variables \code{tripsOrig} and \code{tripsDest}
  #' including aggregated kept and excluded datasets.
  #' @export
  #'
  #' @example
  #' AggregateAllTrips(tripsOrig, tripsDest)
  #'
  AggregateAllTrips <-
    function(tripsOrig, tripsDest = NULL, mapId) {
      # cat(file = stderr(), '3 AggregateAllTrips called',  '\n')
      if (mapId == 'mapOrig') {
        lastIdx <- 0
        if (!is.null(tripsOrig$kept) && nrow(tripsOrig$kept) > 0) {
          agTripsOrig$kept <-
            AggregateTrips(tripsOrig$kept$origin_lat,
                           tripsOrig$kept$origin_lon,
                           lastIdx)
          lastIdx <- nrow(tripsOrig$kept)
        } else {
          agTripsOrig$kept <- NULL
        }
        if (!is.null(tripsOrig$excluded) &&
            nrow(tripsOrig$excluded) > 0) {
          agTripsOrig$excluded <-
            AggregateTrips(tripsOrig$excluded$origin_lat,
                           tripsOrig$excluded$origin_lon,
                           lastIdx)
        } else {
          agTripsOrig$excluded <- NULL
        }
      } else {
        lastIdx <- 10000
        if (!is.null(tripsDest$kept) && nrow(tripsDest$kept) > 0) {
          agTripsDest$kept <-
            AggregateTrips(tripsDest$kept$destination_lat,
                           tripsDest$kept$destination_lon,
                           lastIdx)
          lastIdx <- lastIdx + nrow(tripsDest$kept)
        } else {
          agTripsDest$kept <- NULL
        }
        if (!is.null(tripsDest$excluded) &&
            nrow(tripsDest$excluded) > 0) {
          agTripsDest$excluded <-
            AggregateTrips(
              tripsDest$excluded$destination_lat,
              tripsDest$excluded$destination_lon,
              lastIdx
            )
        } else {
          agTripsDest$excluded <- NULL
        }
      }
    }
}
