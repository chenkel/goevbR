# TODO: Create Function that gets all the shape for all routes.

NormalizeFreq <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  return(as.integer((log2(tripFreq) * 10) ^ (1.2)))
}

GenerateRadius <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  maxTrip <- max(tripFreq)
  maxCircleRadius = 20
  return(maxCircleRadius * tripFreq / maxTrip)
}

GenerateAlpha <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  maxTrip <- max(tripFreq) * 2
  return(tripFreq / maxTrip)
}


GenerateColor <- function(tripFreq) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  maxTrip <- max(tripFreq)
  percentFreq <- tripFreq / maxTrip
  # Hex Lowest: 00b8e5
  # Hex Highest: bf0000
  resultRed <- (0 + percentFreq * (191 - 0)) / 255
  resultGreen <- (184 + percentFreq * (0 - 184)) / 255
  resultBlue <- (229 + percentFreq * (0 - 229)) / 255
  
  return(rgb(resultRed, resultGreen, resultBlue))
}

# SetMapLeafletProxyOptions <- function(proxy) {
#   # TODO: adapt circle size when zooming in.
#
#   return(proxy)
# }

AggregateTrips <- function(lat, lng, startId) {
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
  
  locationTrips$freq_normalized <- NormalizeFreq(locationTrips$freq)
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

SetMapProxy <- function(map, tripData) {
  return(
    leafletProxy(map, data = tripData) %>%
      clearShapes() %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        radius = ~freq_r,
        color = ~freq_c,
        fillOpacity = ~freq_a,
        layerId = ~id,
        stroke = FALSE
      )
  )
}
