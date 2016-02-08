header <- dashboardHeader(title = "Göttingen Bus Trips")

body <- dashboardBody(fluidRow(
  tabBox(
    title = "Busfahren in Göttingen",
    id = "tabsetOrig", width = NULL, height = NULL,
    tabPanel(list(icon("dot-circle-o"), "Start"),(
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL, status = "success", solidHeader = TRUE,
            title = list(icon("calendar-check-o"), "Wochentage filtern..."),
            collapsed = TRUE, collapsible = TRUE,
            checkboxGroupInput(
              "weekdayOrigin", NULL,
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
        ),
        column(
          width = 5,
          box(
            width = NULL, status = "success", solidHeader = FALSE,
            title = list(icon("map-o"), "Haltstellen"),
            leafletOutput("mapOrig", height = 540)
          )
        ),
        column(
          width = 7,
          box(
            width = NULL, status = "success", solidHeader = FALSE,
            title = list(
              icon("tasks"), "Anzahl der Fahrten im aktuellen Kartenausschnitt"
            ),
            plotOutput(
              "histOrigin", height = 540,
              click = "histOriginClick",
              dblclick = "hist_origin_dblclick",
              hover = "hist_origin_hover",
              brush = brushOpts(id = "hist_origin_brush",
                                direction = "y")
            )
          )
        ),
        column(width = 7,
               actionButton("exclude_reset", "Reset"))
      )
    )),
    tabPanel(list(icon("flag-o"), "Ziel"),  (fluidRow(
      column(
        width = 12,
        box(
          width = NULL, status = "danger", solidHeader = TRUE,
          title = "Wochentage filtern...", collapsed = TRUE, collapsible = TRUE,
          checkboxGroupInput(
            "weekdayDestination", NULL,
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
      ),
      column(width = 5,
             box(
               width = NULL, status = "danger",
               leafletOutput("mapDest", height = 580)
             )),
      column(
        width = 7,
        box(
          width = NULL, status = "danger", solidHeader = FALSE,
          title = "Anzahl der Fahrten im aktuellen Kartenausschnitt",
          plotOutput(
            "histDestination", height = 540,
            click = "hist_destination_click",
            dblclick = "hist_destination_dblclick",
            hover = "hist_destination_hover",
            brush = "hist_destination_brush"
          )
        )
      )
    )))
  )
),
detailModal("myModal"))
#               header = textOutput('detailModalTitle', inline = FALSE),
#               content = tags$html(
#                 plotOutput(
#                   "detailHistOrigin",
#                   height = 540
#                 )
#               )))))



sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dashboard", tabName = "dashboard", icon = icon("dashboard")
  ),
  menuItem(
    "Widgets", icon = icon("th"), tabName = "widgets",
    badgeLabel = "new", badgeColor = "green"
  )
))

dashboardPage(dashboardHeader(disable = TRUE),
              dashboardSidebar(disable = TRUE),
              body,
              skin = "black")