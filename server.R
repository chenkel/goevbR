function(input, output, session) {
  # Create the map
  output$mapOrig <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
  })
  output$mapDest <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      # addTiles(urlTemplate = '//{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
  })
  
  # Show a popup at the given location
  showTripPopup <- function(id, lat, lng, mapId) {
    if (mapId == 'mapOrig') {
      theTrip <- tripsOrig$kept[tripsOrig$kept$id == id,]
      stopName <-
        subset(goevb, origin_lat == lat & origin_lon == lng)[, 3][1]
    } else {
      theTrip <-
        tripsDest$kept[tripsDest$kept$id == id,]
      stopName <-
        subset(goevb, destination_lat == lat &
                 destination_lon == lng)[, 4][1]
    }
    
    content <- as.character(
      tagList(
        tags$h4(stopName),
        sprintf('Häufigkeit: %s', theTrip$freq),
        br(),
        br(),
        sprintf('Breite: %s', round(theTrip$latitude, 4)),
        br(),
        sprintf('Länge: %s' ,round(theTrip$longitude, 4)),
        br(),
        br(),
        modalButton("myModal",
                    label = "Details...",
                    icon = icon("server"),)
      )
    )
    #     cat(file = stderr(), 'content: ' , content,  '\n')
    
    leafletProxy(mapId) %>% addPopups(
      lng = theTrip$longitude, lat = theTrip$latitude, popup = content, layerId = theTrip$id
    )
  }
  
  ## Origin map
  
  # When map is clicked, show a popup with trip info
  observeEvent(input$mapOrig_marker_click, ({
    theEvent <- input$mapOrig_marker_click
    
    leafletProxy('mapOrig') %>% clearPopups()
    showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapOrig')
  }))
  
  
  tripsInBoundsOrig <- reactive({
    if (is.null(input$mapOrig_bounds)) {
      return(NULL)
    }
    
    tripFilter$weekday <<- input$weekdayOrigin
    
    bounds <- input$mapOrig_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    trips$keeprows <<-
      (
        goevb$origin_lat >= latRng[1] & goevb$origin_lat <= latRng[2] &
          goevb$origin_lon >= lngRng[1] &
          goevb$origin_lon <= lngRng[2] &
          goevb$day %in% tripFilter$weekday &
          goevb$hour %in% tripFilter$hour
      )
    
    #     goevbFiltered <<-
    #       goevbFiltered[goevbFiltered$day %in% tripFilter$weekday,]
    keptTrips <<- goevb[trips$keeprows, , drop = FALSE]
    cat(file = stderr(), 'n keptTrips', nrow(keptTrips),  '\n')
    excludedTrips <<- goevb[!trips$keeprows, , drop = FALSE]
    
    
    if (shouldRedrawMapOrig == TRUE) {
      cat(file = stderr(), 'Should recalculate because of checkbox changes',  '\n')
      AggregateAllTrips(keptTrips, excludedTrips)
      RedrawMap('mapOrig')
      RedrawMap('mapDest')
      shouldRedrawMapOrig <<- FALSE
    }
    
  })
  
  filterKeptTripsForStop <- function(lat, lng) {
    return(subset(keptTrips, origin_lat == lat & origin_lon == lng))
  }
  
  observeEvent(input$weekdayOrigin, ({
    shouldRedrawMapOrig <<- TRUE
  }))
  
  # Precalculate the breaks we'll need for the  histograms
  
  output$histOrigin <- renderPlot({
    tripsInBoundsOrig()
    if (nrow(keptTrips) < 1) {
      return(NULL)
    }
    ggplot(
      keptTrips, aes(x = hour, fill = day_f),
      cex.lab = 3, cex.axis = 3, cex.main = 1.5, cex.sub = 1.5
    )  +
      geom_bar() +
      coord_flip() +
      scale_x_continuous(limits = c(-0.5, 23.5),
                         breaks = c(0:23)) +
      scale_fill_discrete(name = "Tag") +
      xlab("Uhrzeit") +
      ylab("Häufigkeit") +
      theme(text = element_text(
        size = 17, family = "Source Sans Pro", colour = '#444444'
      )) + 
      scale_fill_brewer(palette = "Greens", name = "Tag")
  })
  
  output$detailHistOrigin <- renderPlot({
    if (nrow(keptTripsForStop) < 1) {
      return(NULL)
    }
    ggplot(
      keptTripsForStop, aes(x = hour, fill = day_f),
      cex.lab = 3, cex.axis = 3, cex.main = 1.5, cex.sub = 1.5
    )  +
      geom_bar() +
      coord_flip() +
      # ylim(0, length(goevbFiltered[,1])) +
      scale_x_continuous(limits = c(-0.5, 23.5),
                         breaks = c(0:23)) +
      xlab("Uhrzeit") +
      ylab("Häufigkeit") +
      theme(text = element_text(
        size = 17, family = "Source Sans Pro", colour = '#444444'
      )) + 
      scale_fill_brewer(palette = "Greens", name = "Tag")
  })
  
  observeEvent(input$hist_origin_brush, {
    cat(file = stderr(), 'input$hist_origin_brush called',  '\n')
    
    ymin <- trunc(input$hist_origin_brush$ymin + 0.5)
    ymax <- trunc(input$hist_origin_brush$ymax + 0.5)
    cat(file = stderr(), 'ymin', ymin,  '\n')
    cat(file = stderr(), 'ymax', ymax,  '\n')
    
    tripFilter$hour <<- c(ymin:ymax)
    shouldRedrawMapOrig <<- TRUE
    # todo: DOES NOT GET CALLED!! WHY!???
    # tripsInBoundsOrig()
    #     res <-
    #       brushedPoints(
    #         keptTrips, input$hist_origin_brush, yvar = 'hour_f', xvar = 'day_f', allRows = TRUE
    #       )
    # trips$keeprows <<- xor(trips$keeprows, res$selected_)
  })
  
  # ## Destination map
  
  observeEvent(input$mapDest_marker_click, ({
    markerEvent <- input$mapDest_marker_click
    
    leafletProxy('mapDest') %>% clearPopups()
    keptTripsForStop <<-
      filterKeptTripsForStop(markerEvent$lat, markerEvent$lng)
    showTripPopup(markerEvent$id, markerEvent$lat, markerEvent$lng, 'mapDest')
  }))
  
  RedrawMap <- function(mapId) {
    if (mapId == 'mapOrig') {
      theData <- tripsOrig
    } else {
      theData <- tripsDest
    }
    return(
      leafletProxy(mapId, data = theData) %>%
        clearMarkers() %>%
        addCircleMarkers(
          theData$excluded$longitude,
          theData$excluded$latitude,
          radius = theData$excluded$freq_r,
          color = "yellow",
          fillOpacity = 0.1,
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
        )
    )
  }
  
  observeEvent(input$exclude_reset, {
    tripFilter$hour <- c(0:23)
    tripFilter$weekday <- c(0:6)
    trips$keeprows <- rep(TRUE, nrow(goevb))
    
  })
}