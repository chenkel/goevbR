header <- dashboardHeader(title = "Göttingen Bus Trips")

body <- dashboardBody(fluidRow(column(
  width = 9,
  box(
    width = NULL, solidHeader = TRUE,
    leafletOutput("map", height = 500)
  )
),
column(
  width = 3,
  box(
    width = NULL, status = "warning",
    radioButtons(
      "selectedWeekday", "Wochentag",
      choices = c(
        "Montag" = 1,
        "Dienstag" = 2,
        "Mittwoch" = 3,
        "Donnerstag" = 4,
        "Freitag" = 5,
        "Samstag" = 6,
        "Sonntag" = 0
      ),
      selected = c(1)
    ),
    p(class = "text-muted",
      paste("Note: a meaningful note here")),
    radioButtons(
      "selectedTime", "Uhrzeit",
      choices = c(
        "8:00 - 12:00" = 0,
        "12:00 - 16:00" = 1,
        "16:00 - 20:00" = 2,
        "20:00 - 24:00" = 3
      ),
      selected = c(0)
    ),
    p(class = "text-muted",
      paste("Note: a meaningful note here")),
    actionButton("zoomButton", "Zoom to fit buses")
  )
)))

dashboardPage(header,
              dashboardSidebar(disable = TRUE),
              body)