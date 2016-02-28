body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "customstyles.css"),
    tags$script(src = "app.js"),
    useShinyjs(),
    extendShinyjs('www/mapsync.js'),
    extendShinyjs('www/dayfilter.js')
  ),
  tabBox(
    title = list(icon("bus"), "Busfahren in Göttingen"),
    id = "tabsetOrig",
    width = NULL,
    height = NULL,
    tabPanel(title = list(icon("dot-circle-o"), "Start"), value = 1, (fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          status = "primary",
          solidHeader = FALSE,
          title = list(icon("map-o"), "Haltestellen"),
          leafletOutput("mapOrig", height = 540)
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          status = "primary",
          solidHeader = FALSE,
          title = list(
            icon("tasks"),
            "Verteilung der Fahrten des sichtbaren Kartenausschnitts     (",
            icon("question-circle"),
             ")"
          ),
          plotOutput(
            "histOrigin",
            height = 540,
            click = "hist_origin_click",
            dblclick = "hist_origin_dblclick",
            hover = "hist_origin_hover",
            brush = brushOpts(id = "hist_origin_brush",
                              direction = "y")
          )
        )
      ),
      # column(width = 7,
      #        actionButton("exclude_reset", "Reset"))
      column(
        width = 12,
        box(
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          title = list(icon("calendar-check-o"), "Wochentage filtern..."),
          collapsed = FALSE,
          collapsible = TRUE,
          actionButton("day2Orig", "Nur Wochentage (Mo-Fr)"),
          actionButton("day3Orig", "Nur Wochenende (Sa-So)"),
          actionButton("day0Orig", "Alle Tage ausschließen ( )"),
          actionButton("day1Orig", "Zurücksetzen (Mo-So)"),
          br(),
          checkboxGroupInput(
            "weekdayOrig",
            NULL,
            c(
              "Montag" = 1,
              "Dienstag" = 2,
              "Mittwoch" = 3,
              "Donnerstag" = 4,
              "Freitag" = 5,
              "Samstag" = 6,
              "Sonntag" = 7
            ),
            inline = TRUE,
            selected = c(1, 2, 3, 4, 5, 6, 7)
          )

        )
      )
    ))),
    tabPanel(title = list(icon("flag-o"), "Ziel"), value = 2,  (fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          status = "danger",
          solidHeader = FALSE,
          title = list(icon("map-o"), "Haltestellen"),
          leafletOutput("mapDest", height = 540)
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          status = "danger",
          solidHeader = FALSE,
          title = list(
            icon("tasks"),
            "Verteilung der Fahrten des sichtbaren Kartenausschnitts     (",
            icon("question-circle"),
            ")"
          ),
          plotOutput(
            "histDestination",
            height = 540,
            click = "hist_destination_click",
            dblclick = "hist_destination_dblclick",
            hover = "hist_destination_hover",
            brush = brushOpts(id = "hist_destination_brush",
                              direction = "y")

          )
        )
      ),
      column(
        width = 12,
        box(
          width = NULL,
          status = "danger",
          solidHeader = TRUE,
          title = list(icon("calendar-check-o"), "Wochentage filtern..."),
          collapsed = FALSE,
          collapsible = TRUE,
          actionButton("day2Dest", "Nur Wochentage (Mo-Fr)"),
          actionButton("day3Dest", "Nur Wochenende (Sa-So)"),
          actionButton("day0Dest", "Alle Tage ausschließen ( )"),
          actionButton("day1Dest", "Zurücksetzen (Mo-So)"),
          br(),
          checkboxGroupInput(
            "weekdayDest",
            NULL,
            c(
              "Montag" = 1,
              "Dienstag" = 2,
              "Mittwoch" = 3,
              "Donnerstag" = 4,
              "Freitag" = 5,
              "Samstag" = 6,
              "Sonntag" = 7
            ),
            inline = TRUE,
            selected = c(1, 2, 3, 4, 5, 6, 7)
          )
        )
      )
    )))
  ),
  detailModal(
    "origModal",
    header = textOutput('detailModalOrigTitle',
                        inline = FALSE),
    content = tags$html(plotOutput("detailHistOrig",
                                   height = 540))
  ),
  detailModal(
    "destModal",
    header = textOutput('detailModalDestTitle',
                        inline = FALSE),
    content = tags$html(plotOutput("detailHistDest",
                                   height = 540))
  )
)

dashboardPage(dashboardHeader(disable = TRUE),
              dashboardSidebar(disable = TRUE),
              body,
              skin = "black")