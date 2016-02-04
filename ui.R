header <- dashboardHeader(title = "Göttingen Bus Trips")
body <-
  dashboardBody(
    fluidRow(column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Start",
        leafletOutput("mapOrigin", height = 500)
      )
    ),
    column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Ziel",
        leafletOutput("mapDestination", height = 500)
      )
    )),
    fluidRow(column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Wochentage",
        checkboxGroupInput(
          "weekdayOrigin", "Wochetag: ",
          c(
            "Montag" = 1,
            "Dienstag" = 2,
            "Mittwoch" = 3,
            "Donnerstag" = 4,
            "Freitag" = 5,
            "Samstag" = 6,
            "Sonntag" = 0
          )
        )
      )
    ),
    column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Wochtage",
        checkboxGroupInput(
          "weekdayDestination", "Wochetag: ",
          c(
            "Montag" = 1,
            "Dienstag" = 2,
            "Mittwoch" = 3,
            "Donnerstag" = 4,
            "Freitag" = 5,
            "Samstag" = 6,
            "Sonntag" = 0
          )
        )
      )
    )),
    fluidRow(column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Histogram",
        plotOutput(
          "histOrigin", height = 250,
          click = "histOriginClick",
          dblclick = "hist_origin_dblclick",
          hover = "hist_origin_hover",
          brush = "hist_origin_brush"
        )
      )
    ),
    column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Histogram",
        plotOutput(
          "histDestination", height = 250,
          click = "hist_destination_click",
          dblclick = "hist_destination_dblclick",
          hover = "hist_destination_hover",
          brush = "hist_destination_brush"
        )
      )
    )),
    fluidRow(column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Konsole",
        textOutput('consoleOrigin')
      )
    ),
    column(
      width = 6,
      box(
        width = NULL, status = "info", solidHeader = TRUE,
        title = "Konsole",
        textOutput('consoledestination')
      )
    ))
  )


dashboardPage(header,
              dashboardSidebar(disable = TRUE),
              body,
              skin = "black")