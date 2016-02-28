# ## Destination map
# ## Set name of chosen stop as modal title
output$detailModalDestTitle <-
  renderText(paste('Ziel:', chosenStopDest$name))

# observeEvent(input$mapDest_marker_click, ({
#   #cat(file = stderr(), 'mapDest_marker_click observeEvent called',  '\n')
#   markerEvent <- input$mapDest_marker_click
#
#   leafletProxy('mapDest') %>% clearPopups()
#   showTripPopup(markerEvent$id,
#                 markerEvent$lat,
#                 markerEvent$lng,
#                 'mapDest')
# }))
#
# # ## Set name of chosen stop as modal title
# output$detailModalDestTitle <- renderText(chosenStopDest$name)

# When map is clicked, show a popup with trip info
observeEvent(input$mapDest_marker_click, ({
  #cat(file = stderr(), 'mapDest_marker_click clicked',  '\n')
  theEvent <- input$mapDest_marker_click
  chosenStopDest$trips <-
    (
      subset(
        tripsDest$kept,
        destination_lat == theEvent$lat &
          destination_lon == theEvent$lng
      )
    )
  chosenStopDest$name <- chosenStopDest$trips[1, 4]
  leafletProxy('mapDest') %>% clearPopups()
  showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapDest')
}))


tripsInBoundsDest <- reactive({
  if (is.null(input$mapDest_bounds)) {
    return(goevb[FALSE,])
  }
  
  tripFilterDest$weekday <- input$weekdayDest
  
  bounds <- input$mapDest_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  isolate({
    shouldKeepDest$flag <-
      (
        goevb$destination_lat >= latRng[1] &
          goevb$destination_lat <= latRng[2] &
          goevb$destination_lon >= lngRng[1] &
          goevb$destination_lon <= lngRng[2] &
          goevb$day %in% tripFilterDest$weekday &
          goevb$hour %in% tripFilterDest$hour
      )
    
    tripsDest$kept <- goevb[shouldKeepDest$flag, , drop = FALSE]
    tripsDest$excluded <- goevb[!shouldKeepDest$flag, , drop = FALSE]
    
    AggregateAllTrips(NULL, tripsDest, 'mapDest')
    RedrawMap('mapDest')
  })
})

#
#
# observeEvent(input$weekdaydestination, ({
#   shouldRedrawMapDest <- TRUE
# }))

# Precalculate the breaks we'll need for the  histograms

output$histDestination <- renderPlot({
  #cat(file = stderr(), 'histDestination renderPlot called',  '\n')
  tripsInBoundsDest()
  
  if (nrow(tripsDest$kept) == 0) {
    return(goevb[FALSE,])
  }
  if (is.null(input$hist_destination_brush) &&
      length(tripFilterDest$hour) > 1) {
    tripFilterDest$hour <- c(0:23)
  }
  
  ggplot(
    tripsDest$kept,
    aes(x = hour, fill = day_f),
    cex.lab = 3,
    cex.axis = 3,
    cex.main = 1.5,
    cex.sub = 1.5
  )  +
    geom_histogram(binwidth = 1, na.rm = TRUE) +
    coord_flip() +
    scale_x_continuous(limits = c(-0.5, 23.5),
                       breaks = c(0:23)) +
    xlab("Uhrzeit") +
    ylab("Häufigkeit") +
    theme(
      text = element_text(
        size = 17,
        family = "Source Sans Pro",
        colour = '#444444'
      ),
      legend.position = c(1, 1),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "lines"),
      plot.margin = unit(c(1, 5, 0.5, 0.5), "lines")
    ) +
    # custom color palette
    scale_fill_brewer(palette = "Accent", name = "Tag")
})

output$detailHistDest <- renderPlot({
  #cat(file = stderr(), 'detailHistdestination renderPlot called',  '\n')
  if (nrow(chosenStopDest$trips) == 0) {
    return(goevb[FALSE,])
  }
  ggplot(
    chosenStopDest$trips,
    aes(x = hour, fill = day_f),
    cex.lab = 3,
    cex.axis = 3,
    cex.main = 1.5,
    cex.sub = 1.5
  )  +
    geom_histogram(binwidth = 1, na.rm = TRUE) +
    coord_flip() +
    # ylim(0, length(goevbFiltered[,1])) +
    scale_x_continuous(limits = c(-0.5, 23.5),
                       breaks = c(0:23)) +
    xlab("Uhrzeit") +
    ylab("Häufigkeit") +
    theme(text = element_text(
      size = 17,
      family = "Source Sans Pro",
      colour = '#444444'
    )) + scale_fill_brewer(palette = "Accent", name = "Tag")
})

observeEvent(input$hist_destination_brush, {
  #cat(file = stderr(), 'input$hist_destination_brush called',  '\n')
  
  ymin <- trunc(input$hist_destination_brush$ymin + 0.5)
  ymax <- trunc(input$hist_destination_brush$ymax + 0.5)
  #cat(file = stderr(), 'ymin', ymin,  '\n')
  #cat(file = stderr(), 'ymax', ymax,  '\n')
  
  tripFilterDest$hour <- c(ymin:ymax)
})

# When a double-click happens, check if there's no brush on the plot.
# If so, select hour.
observeEvent(input$hist_destination_click, {
  if (is.null(input$hist_destination_brush)) {
    click <- trunc(input$hist_destination_click$y + 0.5)
    if (click >= 0 && click <= 23) {
      tripFilterDest$hour <- c(click:click)
    }
  }
})

# When a double-click happens, reset the selection
observeEvent(input$hist_destination_dblclick, {
  tripFilterDest$hour <- c(0:23)
})
