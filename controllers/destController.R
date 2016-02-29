# # Destination map controller

# ## Shiny outputs
# ## --- begin ---
# ### Create the leaflet destination map
output$mapDest <- renderLeaflet({
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    setView(lng = 9.925,
            lat = 51.54,
            zoom = 12) %>%
    addControl(html = tags$div(a(icon('globe'), 'wie Start')),
               position = "bottomleft",
               layerId = 'syncDest')
})

# ### Render the destination bar plot
output$histDestination <- renderPlot({
  tripsInBoundsDest()
  
  if (nrow(tripsDest$kept) == 0) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Es wurde kein Datensatz mit den ausgewählten Filtern gefunden.\n\n",
                                 "- Zoomen Sie aus der Karte heraus um den Ortsfilter zurückzusetzen.\n",
                                 "- Doppelklicken Sie auf diesen Text um den Uhrzeitfilter zurückzusetzen.\n",
                                 "- Setzen Sie den Wochentagsfilter zurück.\n"),
         cex = 1.3, col = "black")
    return()
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

# ### Render the detail destination bar plot
output$detailHistDest <- renderPlot({
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

# ## Set name of chosen stop as modal title
output$detailModalDestTitle <-
  renderText(paste('Ziel:', chosenStopDest$name))
# ## --- end ---
# ## Shiny outputs


# ## Observers and reactive environments
# ## --- begin ---
# ### When map is clicked, show a popup with trip info
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
  chosenStopDest$name <- chosenStopDest$trips[1, 2]
  leafletProxy('mapDest') %>% clearPopups()
  showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapDest')
}))

# ### React to filter changes
tripsInBoundsDest <- reactive({
  if (is.null(input$mapDest_bounds)) {
    return(goevb[FALSE,])
  }
  
  # weekday filter
  tripFilterDest$weekday <- input$weekdayDest
  # hour filter
  tripFilterDest$hour <- tripFilterDest$hour
  
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

# ### React to brush events on bar plot.
observeEvent(input$hist_destination_brush, {
  ymin <- trunc(input$hist_destination_brush$ymin + 0.5)
  ymax <- trunc(input$hist_destination_brush$ymax + 0.5)
  tripFilterDest$hour <- c(ymin:ymax)
})

# ### React to click events on bar plot.
observeEvent(input$hist_destination_click, {
  # When a double-click happens, check if there's no brush on the plot.
  # If so, select hour.
  if (is.null(input$hist_destination_brush)) {
    click <- trunc(input$hist_destination_click$y + 0.5)
    if (click >= 0 && click <= 23) {
      tripFilterDest$hour <- c(click:click)
    }
  }
})

# ### React to double click events on bar plot.
observeEvent(input$hist_destination_dblclick, {
  # When a double-click happens, reset the selection
  tripFilterDest$hour <- c(0:23)
})
# ## --- end ---
# ## Observers and reactive environments


# ## Setup listeners for weekday and sync map buttons
# ## --- begin ---
# ### JS Listeners for weekday buttons
onclick("day0Dest", {
  js$filterDays('0', 'weekdayDest')
})

onclick("day1Dest", {
  js$filterDays('1', 'weekdayDest')
})

onclick("day2Dest", {
  js$filterDays('2', 'weekdayDest')
})

onclick("day3Dest", {
  js$filterDays('3', 'weekdayDest')
})

# ### JS Listeners for map sync buttons
onclick("syncDest", {
  js$syncMap('mapDest')
})
# ## --- end ---
# ## Setup listeners for weekday and sync map buttons