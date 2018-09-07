library(shiny)
library(shinydashboard)
library(leaflet)

SINGAPORE_LONGITUDE <- 103.8198
SINGAPORE_LATITUDE <- 1.3521

map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = SINGAPORE_LONGITUDE,
             lat = SINGAPORE_LATITUDE
             )

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    box(map)
  )
)