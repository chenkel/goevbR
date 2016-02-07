NormalizeFreq <- function(tripFreq) {
  return(as.integer((log2(tripFreq) * 10) ^ (1.2)))
}

GenerateRadius <- function(tripFreq) {
  maxTrip <- max(tripFreq)
  maxCircleRadius = 20
  return(maxCircleRadius * tripFreq / maxTrip)
}

GenerateAlpha <- function(tripFreq) {
  maxTrip <- max(tripFreq) * 2
  return(tripFreq / maxTrip)
}


GenerateColor <- function(tripFreq) {
  maxTrip <- max(tripFreq)
  percentFreq <- tripFreq / maxTrip
  # Hex Lowest: 00b8e5
  # Hex Highest: bf0000
  resultRed <- (0 + percentFreq * (191 - 0)) / 255
  resultGreen <- (184 + percentFreq * (0 - 184)) / 255
  resultBlue <- (229 + percentFreq * (0 - 229)) / 255
  
  return(rgb(resultRed, resultGreen, resultBlue))
}



AggregateTrips <- function(lat, lng, startId) {
  cat(file = stderr(), 'Aggregates Trips',  '\n')
  cat(file = stderr(), 'l Trips', length(lat), '\n')
  if (is.null(lat)  || length(lat) < 1) {
    cat(file = stderr(), 'Lat or Lng is empty',  '\n')
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

AggregateAllTrips <- function(keptTrips, excludedTrips = NULL) {
  # Aggregates all trips with the same origin latitude and longitude and count them.
  # cat(file = stderr(), '!isNull', ,  '\n')
  if (!is.null(keptTrips)  &&
      length(keptTrips) > 0 && nrow(keptTrips) > 0) {
    tripsOrig$kept <<-
      AggregateTrips(keptTrips$origin_lat, keptTrips$origin_lon, 0)
    tripsOrigLength <- nrow(tripsOrig$kept)
    tripsDest$kept <<-
      AggregateTrips(keptTrips$destination_lat, keptTrips$destination_lon, tripsOrigLength)
  } else {
    tripsOrig$kept$freq_r <<- 0
    tripsDest$kept$freq_r <<- 0
  }
  
  if (!is.null(excludedTrips) &&
      length(excludedTrips) > 0 && nrow(excludedTrips) > 0) {
    tripsOrig$excluded <<-
      AggregateTrips(excludedTrips$origin_lat, excludedTrips$origin_lon, 0)
    tripsOrigLength <- nrow(tripsOrig$excluded)
    tripsDest$excluded <<-
      AggregateTrips(
        excludedTrips$destination_lat, excludedTrips$destination_lon, tripsOrigLength
      )
  } else {
    tripsOrig$excluded$freq_r <<- 0
    tripsDest$excluded$freq_r <<- 0
  }
  
}
