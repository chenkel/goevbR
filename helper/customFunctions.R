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
  maxTrip <- max(tripFreq) * 1.4
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
  theColorPal <- brewer.pal(5, 'YlOrRd')
  theBreaks <- 5
  nTripsFreq <- length(tripFreq)
  if (nTripsFreq == 1) {
    theColors <- theColorPal[3]
  } else if (nTripsFreq > 1) {
    theBreaks <- min(c(nTripsFreq, 5))
    theColors <-
      theColorPal[as.numeric(cut(x = tripFreq, breaks = theBreaks))]
  }
  
  return(theColors)
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
            "Schließen"
          )
        )
      )
    )
  )
  tags$html(modal)
}
