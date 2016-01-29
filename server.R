function(input, output, session) {
  # Create the map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles(urlTemplate = "//{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png") %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
    map
  })
  
  
  tripsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(locationTrips[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(
      locationTrips,
      origin_lat >= latRng[1] & origin_lat <= latRng[2] &
        origin_lon >= lngRng[1] & origin_lon <= lngRng[2]
    )
  })
  
  # Precalculate the breaks we'll need for the  histograms
  # TODO: Display on the right side of the dashboard
  freqBreaks <-
    hist(plot = FALSE, locationTrips$freq, breaks = 30)$breaks
  
  output$histFreq <- renderPlot({
    if (nrow(tripsInBounds()) == 0)
      return(NULL)
    
    hist(
      tripsInBounds()$freq,
      breaks = freqBreaks,
      main = "Trip count (visible trips)",
      xlab = "Trip",
      xlim = range(locationTrips$freq),
      col = '#00DD00',
      border = 'white'
    )
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # TODO: change dataset according to variable choice from dropdown list.
  observe({
    dayBy <- input$day
    hourBy <- input$hour
    
    leafletProxy("map", data = locationTrips) %>%
      clearShapes() %>%
      addCircles(
        ~ origin_lon,
        ~ origin_lat,
        radius = 175,
        layerId = ~ origin_lon,
        stroke = FALSE,
        color = ~ freq_c,
        fillOpacity = ~ freq_a
      )
  })
  
  # Show a popup at the given location
  showTripPopup <- function(lon, lat, lng) {
    selectedTrip <- goevb[goevb$origin_lon == lon,]
    selectedTrip <- selectedTrip[1,]
    content <- as.character(
      tagList(
        tags$h4("Trip ID:", as.integer(selectedTrip$id)),
        sprintf(
          "Origin: %s, Latitude: %s Longitude: %s",
          selectedTrip$origin, selectedTrip$origin_lat, selectedTrip$origin_lon
        )
        , tags$br(),
        sprintf(
          "Destination: %s, Latitude: %s Longitude: %s", selectedTrip$destination, selectedTrip$destination_lat, selectedTrip$destination_lon
        ), tags$br(),
        sprintf("Bus line(s): %s", selectedTrip$line), tags$br(),
        sprintf("Datetime: %s", selectedTrip$datetime)
      )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = lon)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showTripPopup(event$id, event$lat, event$lng)
    })
  })
  
}
