function(input, output, session) {
  # Create the map
  output$mapOrigin <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
  })
  output$mapDestination <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      # addTiles(urlTemplate = '//{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # TODO: change dataset according to variable choice from dropdown list.
  observe({
    dayBy <- input$day
    hourBy <- input$hour
    
    SetMapProxy('mapOrigin', locationTripsOrigin)
    SetMapProxy('mapDestination', locationTripsDestination)
  })
  
  # Show a popup at the given location
  showTripPopup <- function(id, lat, lng, mapId) {
    if (mapId == 'mapOrigin') {
      theTrip <- locationTripsOrigin[locationTripsOrigin$id == id,]
    } else {
      theTrip <-
        locationTripsDestination[locationTripsDestination$id == id,]
    }
    
    content <- as.character(tagList(
      tags$h4('Trip ID:', as.integer(theTrip$id)),
      sprintf(
        'Freq: %s, Latitude: %s Longitude: %s',
        theTrip$freq, theTrip$latitude, theTrip$longitude
      )
    ))
    #     cat(file = stderr(), 'content: ' , content,  '\n')
    
    leafletProxy(mapId) %>% addPopups(
      lng = theTrip$longitude, lat = theTrip$latitude, popup = content, layerId = theTrip$id
    )
  }
  
  ## Origin map
  
  # When map is clicked, show a popup with trip info
  observeEvent(input$mapOrigin_marker_click, ({
    theEvent <- input$mapOrigin_marker_click
    
    leafletProxy('mapOrigin') %>% clearPopups()
    showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapOrigin')
  }))
  
  tripsInBounds <- reactive({
    if (is.null(input$mapOrigin_bounds)) {
      return(locationTripsOrigin[FALSE,])
    }
    
    
    weekdayFilter <- input$weekdayOrigin
    cat(file = stderr(), 'weekdayFilter: ', weekdayFilter, '\n')
    
#     goevbFilteredByDay = goevb[grep() ]
#     
#     goevb$
    
    
    bounds <- input$mapOrigin_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      goevb,
      origin_lat >= latRng[1] & origin_lat <= latRng[2] &
        origin_lon >= lngRng[1] & origin_lon <= lngRng[2] 
    )
  })
  
  # Precalculate the breaks we'll need for the  histograms
  
  output$histOrigin <- renderPlot({
    if (nrow(tripsInBounds()) == 0) {
      
      return(NULL)
    }
    ggplot(tripsInBounds(), aes(x = factor(hour))) + geom_bar()
    
  })

  output$consoleOrigin <- renderText({
    
    input$action # makes sure nothing moves till the button is hit
    # isolate prevents datasetInput from reactively evaluating
    input$weekdayOrigin 
    
#     xy_str <- function(e) {
#       if (is.null(e)) {
#         return("NULL\n")
#       }
#       paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
#     }
#     xy_range_str <- function(e) {
#       if (is.null(e)) {
#         return("NULL\n")
#       }
#       paste0(
#         "xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
#         " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1)
#       )
#     }
#     
#     paste0(
#       "click: ", xy_str(input$histOriginClick),
#       "dblclick: ", xy_str(input$hist_origin_dblclick),
#       "hover: ", xy_str(input$hist_origin_hover),
#       "brush: ", xy_range_str(input$hist_origin_brush)
#     )
  })
  
  # ## Destination map
  
  observeEvent(input$mapDestination_marker_click, ({
    theEvent <- input$mapDestination_marker_click
    
    leafletProxy('mapDestination') %>% clearPopups()
    showTripPopup(theEvent$id, theEvent$lat, theEvent$lng, 'mapDestination')
  }))
}