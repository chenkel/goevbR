#layout of the shiny dashboard

#generate header of dashboard
header<- dashboardHeader(title = "Göttingen Bus Trips")

#generate body component of the dashboard
body <- dashboardBody(fluidRow(
column(width = 6,
  box(title = "Origin points", status = "success",width = NULL, solidHeader = TRUE,
    leafletOutput("map", height = 500))),
column(width = 6,
  box(title = "Destination points", status = "success",width = NULL, solidHeader = TRUE,
    leafletOutput("map2", height = 500)))))

#summary of layout of dashboard, sidebar disables
dashboardPage(skin="green", header,
              dashboardSidebar(disable = TRUE),
              body)