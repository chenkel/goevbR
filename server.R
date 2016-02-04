function(input, output, session) {
  # Create the map
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles(urlTemplate = "//{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png") %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
    map
  })
  output$map2 <- renderLeaflet({
    map2 <- leaflet() %>%
      addTiles(urlTemplate = "//{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png") %>%
      setView(lng = 9.925, lat = 51.54, zoom = 12)
    map2
  })
  
  # TODO: Use for dynamic histogram
#   tripsInBounds <- reactive({
#     if (is.null(input$map_bounds))
#       return(locationTrips[FALSE,])
#     bounds <- input$map_bounds
#     latRng <- range(bounds$north, bounds$south)
#     lngRng <- range(bounds$east, bounds$west)
#     
#     subset(
#       locationTrips,
#       origin_lat >= latRng[1] & origin_lat <= latRng[2] &
#         origin_lon >= lngRng[1] & origin_lon <= lngRng[2]
#     )
#   })
#   
#   # Precalculate the breaks we'll need for the  histograms
#   # TODO: Display on the right side of the dashboard
#   freqBreaks <-
#     hist(plot = FALSE, locationTrips$freq, breaks = 30)$breaks
#   
#   output$histFreq <- renderPlot({
#     if (nrow(tripsInBounds()) == 0)
#       return(NULL)
#     
#     hist(
#       tripsInBounds()$freq,
#       breaks = freqBreaks,
#       main = "Trip count (visible trips)",
#       xlab = "Trip",
#       xlim = range(locationTrips$freq),
#       col = '#00DD00',
#       border = 'white'
#     )
#   })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # TODO: change dataset according to variable choice from dropdown list.
  observe({
    dayBy <- input$day
    hourBy <- input$hour
    # TODO: adapt circle size when zooming in.
    leafletProxy("map", data = locationTripsOrigin) %>%
      clearShapes() %>%
      addCircles(
        ~ origin_lon,
        ~ origin_lat,
        radius = ~ freq_r,
        layerId = ~ origin_lon,
        stroke = FALSE,
        color = ~ freq_c,
        fillOpacity = ~ 0.5
      )
    # Extract as function
    leafletProxy("map2", data = locationTripsDestination) %>%
      clearShapes() %>%
      addCircles(
        ~ destination_lon,
        ~ destination_lat,
        radius = ~ freq_r,
        layerId = ~ destination_lon,
        stroke = FALSE,
        color = ~ freq_c,
        fillOpacity = ~ 0.5
      )
    
  })
  
  # Show a popup at the given location at map
  showTripPopup <- function(lon, lat, lng) {
    selectedTrip <- goevb[goevb$origin_lon == lon,]
    selectedTrip <- selectedTrip[1,]
    content <- as.character(
      tagList(
        tags$h4("Bus station:", selectedTrip$origin),
        sprintf(
          "Latitude: %s Longitude: %s",
          selectedTrip$origin_lat, selectedTrip$origin_lon),
        tags$br()
        #TODO: Frequency missing
       # sprintf("Frequency: %s", locationTripsOrigin$freq), tags$br(),
       ))
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
# Show a popup at the given location at map2
#showTripPopup2 <- function(lon, lat, lng)
 # {selectedTrip2 <- goevb[goevb$destination_lon == lon,]
  #selectedTrip2 <- selectedTrip2[1,]
  #content <- as.character
  #(tagList(tags$h4("Bus station:", selectedTrip2$destination),
   #        sprintf("Latitude: %s Longitude: %s",
                   #selectedTrips2$destination_lat, selectedTrip2$destination_lon),
    #  tags$br()))
  #leafletProxy("map2") %>% addPopups(lng, lat, content, layerId = lon)}


# When map2 is clicked, show a popup with city info
#observe({
 # leafletProxy("map2") %>% clearPopups()
 # event2 <- input$map_shape_click
 # if (is.null(event2))
 #   return()
  
 # isolate({
 #   showTripPopup2(event2$id, event2$lat, event2$lng)
 # })
#})

