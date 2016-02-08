NormalizeFreq <- function(tripFreq) {
  cat(file = stderr(), 'NormalizeFreq',  '\n')
  return(as.integer((log2(tripFreq) * 10) ^ (1.2)))
}

GenerateRadius <- function(tripFreq) {
  cat(file = stderr(), 'GenerateRadius',  '\n')
  maxTrip <- max(tripFreq)
  maxCircleRadius = 20
  return(maxCircleRadius * tripFreq / maxTrip)
}

GenerateAlpha <- function(tripFreq) {
  cat(file = stderr(), 'GenerateAlpha',  '\n')
  maxTrip <- max(tripFreq) * 2
  return(tripFreq / maxTrip)
}


GenerateColor <- function(tripFreq) {
  cat(file = stderr(), 'GenerateColor',  '\n')
  maxTrip <- max(tripFreq)
  percentFreq <- tripFreq / maxTrip
  ## Hex Lowest: 00b8e5
  ## Hex Highest: bf0000
  resultRed <- (0 + percentFreq * (191 - 0)) / 255
  resultGreen <- (204 + percentFreq * (0 - 204)) / 255
  resultBlue <- (0 + percentFreq * (0 - 0)) / 255
  
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
  cat(file = stderr(), 'AggregateAllTrips',  '\n')
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


#' modalButton
#'
#' Creates a modal dialog box. \code{header} and \code{content} can
#' include shiny inputs and outputs.
#'
#' @param inputId       Specifies the input slot that will be used to access the value.
#' @param label         The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon          An optional icon to appear on the button.
#' @param header   HTML for the header
#' @param content  HTML for the content
#'
#' @return html for modal button and window
#' @export
#'
#' @example
#' # In ui.R
#' modalButton("helpModal", "", icon = icon("info-circle"),
#'   header = tags$h3("Help for my app"),
#'   content = tags$p("Some detailed help"))
#'
modalButton <-
  function(inputId, label, icon = NULL, header = "", content = "")
  {
    cat(file = stderr(), 'modalButton',  '\n')
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

detailModal <- function(inputId, header = ' ', content = ' ') {
  cat(file = stderr(), 'detailModal',  '\n')
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
