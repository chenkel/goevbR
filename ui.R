header <- dashboardHeader(title = "Göttingen Bus Trips")

body <- dashboardBody(fluidRow(column(
  width = 6,
  box(
    width = NULL, solidHeader = TRUE,
    leafletOutput("map", height = 500)
  )
),
column(
  width = 6,
  box(
    width = NULL, solidHeader = TRUE,
    leafletOutput("map2", height = 500)
  )
)))

dashboardPage(header,
              dashboardSidebar(disable = TRUE),
              body)