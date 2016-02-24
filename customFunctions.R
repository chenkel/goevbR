#' NormalizeFreq
#'
#' Normalizes the frequency of aggregated trips
#'
#' The frequencies of aggregated trips are distributed unevenly.
#' Thus a lot of stations have far more trips than others.
#' In order to display differences on a map, the extremes values need to be trimmed.
#'
#' @param tripFreq  Specifies the unnormalized frequency dataset.

#' @return Normalized frequencies.
#' @export
#'
#' @example
#' NormalizeFreq(locationTrips$freq)
#'
NormalizeFreq <- function(tripFreq) {
  return(as.integer((log2(tripFreq) * 10) ^ (1.2)))
}

#' GenerateRadius
#'
#' Generates radiuses for heatmap circles based on the given frequency dataset.
#'
#' @param tripFreq  Specifies the frequency dataset.

#' @return Radii dataset.
#' @export
#'
#' @example
#' GenerateRadius(locationTrips$freq_normalized)
#'
GenerateRadius <- function(tripFreq) {
  maxTrip <- max(tripFreq)
  maxCircleRadius = 20
  return(maxCircleRadius * tripFreq / maxTrip)
}

#' GenerateAlpha
#'
#' Generates alpha dataset for heatmap circles based on the given frequency dataset.
#'
#' @param tripFreq  Specifies the frequency dataset.

#' @return Dataset with alpha information.
#' @export
#'
#' @example
#' GenerateAlpha(locationTrips$freq_normalized)
#'
GenerateAlpha <- function(tripFreq) {
  maxTrip <- max(tripFreq) * 2
  return(tripFreq / maxTrip)
}

#' GenerateColor
#'
#' Generates color dataset for heatmap circles based on the given frequency dataset.
#'
#' @param tripFreq  Specifies the frequency dataset.

#' @return Dataset with color information.
#' @export
#'
#' @example
#' GenerateColor(locationTrips$freq_normalized)
#'
GenerateColor <- function(tripFreq) {
  maxTrip <- max(tripFreq)
  percentFreq <- tripFreq / maxTrip
  resultRed <- (0 + percentFreq * (191 - 0)) / 255
  resultGreen <- (204 + percentFreq * (0 - 204)) / 255
  resultBlue <- (0 + percentFreq * (0 - 0)) / 255
  
  return(rgb(resultRed, resultGreen, resultBlue))
}


#' AggregateTrips
#'
#' Groups all trips by location and counts the occurrences in these groups
#'
#' @param lat       The dataset containing latitude infromation of all trips.
#' @param lng       The dataset containing longitude infromation of all trips.
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
  cat(file = stderr(), 'AggregateTrips called',  '\n')
  if (is.null(lat)  || length(lat) < 1) {
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
    if (mapId == 'mapOrig') {
      if (!is.null(tripsOrig$kept)) {
      lastIdx <- 0
        agTripsOrig$kept <<-
          AggregateTrips(tripsOrig$kept$origin_lat,
                         tripsOrig$kept$origin_lon,
                         lastIdx)
        lastIdx <- nrow(tripsOrig$kept)
      }
      if (!is.null(tripsOrig$excluded)) {
        agTripsOrig$excluded <<-
          AggregateTrips(tripsOrig$excluded$origin_lat,
                         tripsOrig$excluded$origin_lon,
                         lastIdx)
      }
    } else {
      if (!is.null(tripsDest$kept)) {
        lastIdx <- 10000
        agTripsDest$kept <<-
          AggregateTrips(tripsDest$kept$destination_lat,
                         tripsDest$kept$destination_lon,
                         lastIdx)
        lastIdx <- lastIdx + nrow(tripsDest$kept)
      }
      if (!is.null(tripsDest$excluded)) {
        agTripsDest$excluded <<-
          AggregateTrips(
            tripsDest$excluded$destination_lat,
            tripsDest$excluded$destination_lon,
            lastIdx
          )
      }
    }
  }


#' modalButton
#'
#' Creates a button that opens the detail modal
#'
#' @param inputId       Specifies the input slot that will be used to access the value.
#' @param label         The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon          An optional icon to appear on the button.
#'
#' @return html for modal button and window
#' @export
#'
#' @example
#' modalButton("helpModal", "HELP", icon = icon("info-circle"))
#'
modalButton <-
  function(inputId, label, icon = NULL)
  {
    # create the button
    button <- tags$button(
      type = "button",
      class = "btn btn-info",
      `data-toggle` = "modal",
      `data-target` = paste0("#", inputId, sep = ""),
      list(icon, label)
    )
    
    tags$html(button)
  }


#' detailModal
#'
#' Creates a modal window.
#' \code{header} and \code{content} can include shiny inputs and outputs.
#'
#' @param inputId       Specifies the input slot that will be used to access the value.
#' @param header   HTML for the header
#' @param content  HTML for the content
#'
#' @return html for modal window
#' @export
#'
#' @example
#' # In ui.R
#' detailModal("helpModal", "", icon = icon("info-circle"),
#'   header = textOutput('detailModalTitle', inline = FALSE),
#'   content = tags$html(plotOutput("detailHistOrigin", height = 540)))
#'
detailModal <- function(inputId,
                        header = ' ',
                        content = ' ') {
  # create the window
  modal <- tags$div(
    id = inputId,
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `aria-labelledby` = paste0(inputId, "Label"),
    `aria-hidden` = "false",
    tags$div(
      class = "modal-dialog",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-hidden` = "true",
            "x"
          ),
          tags$h4(header)
        ),
        tags$div(class = "modal-body",
                 content),
        tags$div(
          class = "modal-footer",
          tags$button(
            class = "btn",
            `data-dismiss` = "modal",
            `aria-hidden` = "true",
            "Close"
          )
        )
      )
    )
  )
  tags$html(modal)
}
