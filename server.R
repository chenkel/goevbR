function(input, output, session) {
  # ## Create the leaflet origin map
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
  # ## Create the leaflet destination map
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
  
  onclick("syncOrig", {
    js$syncMap('mapOrig')
  })
  
  onclick("syncDest", {
    js$syncMap('mapDest')
  })
  
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
  
  # ## Retrieves the stop name from given latitude and longitude from the goevb dataset.
  GetStopName <- function(lat, lng, orig0dest1) {
    if (orig0dest1 == 0) {
      return(subset(goevb, origin_lat == lat & origin_lon == lng)[, 3][1])
    } else if (orig0dest1 == 1) {
      return(subset(goevb, destination_lat == lat &
                      destination_lon == lng)[, 4][1])
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
        # Call custom button to show modal
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
  
  ## Origin map
  
  # ## Set name of chosen stop as modal title
  output$detailModalOrigTitle <-
    renderText(paste('Start:', chosenStopOrig$name))
  
  # ## When map is clicked, show a popup with trip info
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
    chosenStopOrig$name <- chosenStopOrig$trips[1, 3]
    # delete all other opened popups
    leafletProxy('mapOrig') %>% clearPopups()
    # load popup content and show it
    showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapOrig')
  }))
  
  # ## Filters trips according to map's bounds, weekday, hour
  tripsInBoundsOrig <- reactive({
    # cat(file = stderr(), '2 tripsInBoundsOrig called',  '\n')
    # sanity checking
    if (is.null(input$mapOrig_bounds)) {
      return(goevb[FALSE,])
    }
    # weekday filter
    tripFilterOrig$weekday <- input$weekdayOrig
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
  
  # observeEvent(input$weekdayOrig, ({
  #   #shouldRedrawMapOrig <<- TRUE
  # }))
  
  # ## Detail bar plot only shows the chosen stop
  output$histOrigin <- renderPlot({
    # cat(file = stderr(), '1 histOrigin called',  '\n')
    tripsInBoundsOrig()
    # sanity checking
    if (nrow(tripsOrig$kept) == 0) {
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
  
  # ## Detail bar plot only shows the chosen stop
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
  
  # ## time filter by brushing the bar plot
  observeEvent(input$hist_origin_brush, {
    # read inputs
    ymin <- trunc(input$hist_origin_brush$ymin + 0.5)
    ymax <- trunc(input$hist_origin_brush$ymax + 0.5)
    
    # set filter
    tripFilterOrig$hour <- c(ymin:ymax)
  })
  
  # When a double-click happens, check if there's no brush on the plot.
  # If so, select hour.
  observeEvent(input$hist_origin_click, {
    if (is.null(input$hist_origin_brush)) {
      click <- trunc(input$hist_origin_click$y + 0.5)
      if (click >= 0 && click <= 23) {
        tripFilterOrig$hour <- c(click:click)
      }
    }
  })
  
  # When a double-click happens, reset the selection
  observeEvent(input$hist_origin_dblclick, {
    tripFilterOrig$hour <- c(0:23)
  })
  
  # similiar to code above... still needs refactoring
  source('destController.R', local = TRUE)
  
  # ## Draws heatmap circels in map
  RedrawMap <- function(mapId) {
    # cat(file = stderr(), '5 mapOrig - RedrawMap called',  '\n')
    
    if (mapId == 'mapOrig') {
      theData <- agTripsOrig
    } else {
      theData <- agTripsDest
    }
    # Sanity checking
    if (!is.null(theData$excluded) || !is.null(theData$kept)) {
      leafletProxy(mapId, data = theData) %>%
        clearMarkers()
    } else {
      return()
    }
    # cat(file = stderr(), 'theData$excluded: ', is.null(theData$excluded), 'theData$kept: ', is.null(theData$kept), '\n')
    if (is.null(theData$excluded) && !is.null(theData$kept)) {
      qpal <- colorBin("YlOrRd", theData$kept$freq, bins = 5)
      return(
        leafletProxy(mapId, data = theData) %>%
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
    } else if (!is.null(theData$excluded) &&
               !is.null(theData$kept)) {
      qpal <-
        colorBin("YlOrRd", theData$kept$freq, bins = 5)      # Draw circles in yellow with precalculated radius
      return(
        leafletProxy(mapId, data = theData) %>%
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
}
