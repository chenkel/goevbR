body <- dashboardBody(
  fluidRow(
    tabBox(
      title = list(icon("bus"), "Busfahren in Göttingen"),
      id = "tabsetOrig",
      width = NULL,
      height = NULL,
      tabPanel(list(icon("dot-circle-o"), "Start"), (fluidRow(
        column(
          width = 5,
          box(
            width = NULL,
            status = "success",
            solidHeader = FALSE,
            title = list(icon("map-o"), "Haltstellen"),
            leafletOutput("mapOrig", height = 540)
          )
        ),
        column(
          width = 7,
          box(
            width = NULL,
            status = "success",
            solidHeader = FALSE,
            title = list(
              icon("tasks"),
              "Verteilung der Fahrten des sichtbaren Kartenausschnitts"
            ),
            plotOutput(
              "histOrigin",
              height = 540,
              click = "histOriginClick",
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
            status = "success",
            solidHeader = TRUE,
            title = list(icon("calendar-check-o"), "Wochentage filtern..."),
            collapsed = TRUE,
            collapsible = TRUE,
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
      tabPanel(list(icon("flag-o"), "Ziel"),  (fluidRow(
        column(
          width = 5,
          box(
            width = NULL,
            status = "danger",
            solidHeader = FALSE,
            title = list(icon("map-o"), "Haltstellen"),
            leafletOutput("mapDest", height = 540)
          )
        ),
        column(
          width = 7,
          box(
            width = NULL,
            status = "danger",
            solidHeader = FALSE,
            title = list(
              icon("tasks"),
              "Verteilung der Fahrten des sichtbaren Kartenausschnitts"
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
            collapsed = TRUE,
            collapsible = TRUE,
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
    )
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



sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem(
    "Widgets",
    icon = icon("th"),
    tabName = "widgets",
    badgeLabel = "new",
    badgeColor = "green"
  )
))

dashboardPage(dashboardHeader(disable = TRUE),
              dashboardSidebar(disable = TRUE),
              body,
              skin = "black")