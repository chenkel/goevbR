# # Origin map controller

# ## Shiny outputs
# ## --- begin ---
# ### Create the leaflet origin map
output$mapOrig <- renderLeaflet({
  leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    setView(lng = 9.925,
            lat = 51.54,
            zoom = 12) %>%
    addControl(html = tags$div(a(icon('globe'), 'wie Ziel')),
               position = "bottomleft",
               layerId = 'syncOrig')
})

# ### Detail bar plot only shows the chosen stop
output$histOrigin <- renderPlot({
  # cat(file = stderr(), '1 histOrigin called',  '\n')
  tripsInBoundsOrig()
  # sanity checking
  if (nrow(tripsOrig$kept) == 0) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Es wurde kein Datensatz mit den ausgewählten Filtern gefunden.\n\n",
                                 "- Zoomen Sie aus der Karte heraus um den Ortsfilter zurückzusetzen.\n",
                                 "- Doppelklicken Sie auf diesen Text um den Uhrzeitfilter zurückzusetzen.\n",
                                 "- Setzen Sie den Wochentagsfilter zurück.\n"),
         cex = 1.3, col = "black")
    return(goevb[FALSE,])
  }
  if (is.null(input$hist_origin_brush) &&
      length(tripFilterOrig$hour) > 1) {
    tripFilterOrig$hour <- c(0:23)
  }
  # Bar Blot to show trip frequencies
  ggplot(
    # show filtered trips dataset
    tripsOrig$kept,
    # stacked bar plot (see fill)
    aes(x = hour, fill = day_f),
    # font sizing
    cex.lab = 3,
    cex.axis = 3,
    cex.main = 1.5,
    cex.sub = 1.5
  )  +
    geom_histogram(binwidth = 1, na.rm = TRUE) +
    # flip it to a horizontal one
    coord_flip() +
    scale_x_continuous(limits = c(-0.5, 23.5),
                       breaks = c(0:23)) +
    # axis labeling
    xlab("Uhrzeit") +
    ylab("Häufigkeit") +
    # custom font (same as ShinyDashboard)
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

# ### Detail bar plot only shows the chosen stop
output$detailHistOrig <- renderPlot({
  # sanity checking
  if (nrow(chosenStopOrig$trips) == 0) {
    return(goevb[FALSE,])
  }
  # Bar Blot to show trip frequencies (similiar to histOrigin, refactoring needed)
  ggplot(
    chosenStopOrig$trips,
    # stacked bar plot (fill)
    aes(x = hour, fill = day_f),
    # font sizing
    cex.lab = 3,
    cex.axis = 3,
    cex.main = 1.5,
    cex.sub = 1.5
  )  +
    geom_histogram(binwidth = 1, na.rm = TRUE) +
    # flip it to a horizontal one
    coord_flip() +
    scale_x_continuous(limits = c(-0.5, 23.5),
                       breaks = c(0:23)) +
    # axis labeling
    xlab("Uhrzeit") +
    ylab("Häufigkeit") +
    # custom font (same as ShinyDashboard)
    theme(text = element_text(
      size = 17,
      family = "Source Sans Pro",
      colour = '#444444'
    )) +
    # custom color palette
    scale_fill_brewer(palette = "Accent", name = "Tag")
})

# ### Set name of chosen stop as modal title
output$detailModalOrigTitle <-
  renderText(paste('Start:', chosenStopOrig$name))
# ## --- end ---
# ## Shiny outputs


# ## Observers and reactive environments
# ## --- begin ---
# ### When map is clicked, show a popup with trip info
observeEvent(input$mapOrig_marker_click, ({
  theEvent <- input$mapOrig_marker_click
  # search chosen stop in dataset
  chosenStopOrig$trips <-
    (subset(
      tripsOrig$kept,
      origin_lat == theEvent$lat &
        origin_lon == theEvent$lng
    ))
  # filterTripsByStop(theEvent$lat, theEvent$lng, 'mapOrig')
  # get name of chosen stop
  chosenStopOrig$name <- chosenStopOrig$trips[1, 1]
  # delete all other opened popups
  leafletProxy('mapOrig') %>% clearPopups()
  # load popup content and show it
  showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapOrig')
}))

# ### Filters trips according to map's bounds, weekday, hour
tripsInBoundsOrig <- reactive({
  # cat(file = stderr(), '2 tripsInBoundsOrig called',  '\n')
  # sanity checking
  if (is.null(input$mapOrig_bounds)) {
    return(goevb[FALSE,])
  }
  # weekday filter
  tripFilterOrig$weekday <- input$weekdayOrig
  # hour filter
  tripFilterOrig$hour <- tripFilterOrig$hour
  # map's bounds filter
  bounds <- input$mapOrig_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  isolate({
    # screen all the data by current filters
    shouldKeepOrig$flag <<-
      (
        goevb$origin_lat >= latRng[1] & goevb$origin_lat <= latRng[2] &
          goevb$origin_lon >= lngRng[1] &
          goevb$origin_lon <= lngRng[2] &
          goevb$day %in% tripFilterOrig$weekday &
          goevb$hour %in% tripFilterOrig$hour
      )
    # construct data according to filter results
    tripsOrig$kept <- goevb[shouldKeepOrig$flag, , drop = FALSE]
    tripsOrig$excluded <-
      goevb[!shouldKeepOrig$flag, , drop = FALSE]
    
    # Aggregate trips to update the heatmap
    AggregateAllTrips(tripsOrig, NULL, 'mapOrig')
    # Update the heatmap
    RedrawMap('mapOrig')
  })
})

# ### time filter by brushing the bar plot
observeEvent(input$hist_origin_brush, {
  # read inputs
  ymin <- trunc(input$hist_origin_brush$ymin + 0.5)
  ymax <- trunc(input$hist_origin_brush$ymax + 0.5)
  
  # set filter
  tripFilterOrig$hour <- c(ymin:ymax)
})

# ### React to click events on bar plot.
observeEvent(input$hist_origin_click, {
  # When a double-click happens, check if there's no brush on the plot.
  # If so, select hour.
  if (is.null(input$hist_origin_brush)) {
    click <- trunc(input$hist_origin_click$y + 0.5)
    if (click >= 0 && click <= 23) {
      tripFilterOrig$hour <- c(click:click)
    }
  }
})

# ### React to double click events on bar plot.
observeEvent(input$hist_origin_dblclick, {
  # When a double-click happens, reset the selection
  tripFilterOrig$hour <- c(0:23)
})
# ## --- end ---
# ## Observers and reactive environments


# ## Setup listeners for weekday and sync map buttons
# ## --- begin ---
# ### JS Listeners for weekday buttons
onclick("day0Orig", {
  js$filterDays('0', 'weekdayOrig')
})

onclick("day1Orig", {
  js$filterDays('1', 'weekdayOrig')
})

onclick("day2Orig", {
  js$filterDays('2', 'weekdayOrig')
})

onclick("day3Orig", {
  js$filterDays('3', 'weekdayOrig')
})

# ### JS Listeners for map sync buttons
onclick("syncOrig", {
  js$syncMap('mapOrig')
})
# ## --- end ---
# ## Setup listeners for weekday and sync map buttons
