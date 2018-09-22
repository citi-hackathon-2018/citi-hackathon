library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)

DEFAULT_BOX_HEIGHT = NULL
PROPERTY_DATA_COLUMN_NAMES = c('Region', 'Apartment Type', 'Block', 'Street Name', 'Floor', 
                               'Size', 'Price', 'Address', 'Age', 'Longitude', 'Latitude')
PROPERTY_COORDINATE_COLUMN_POSITIONS = c(10, 11)
PROPERTY_INFO_COLUMN_POSITIONS = c(1 : 9)
data = read.csv('data/properties.csv', header = T)
colnames(data) = PROPERTY_DATA_COLUMN_NAMES
apartment_type = levels(data$'Apartment Type')
region = levels(data$'Region')

filters <- box(width = 2,
               height = DEFAULT_BOX_HEIGHT,
               background = 'maroon',
               solidHeader = T,
               title = "Filters",
               numericInput('MonthlyIncome', "Monthly Income (Combined)", 3000),
               numericInput('CPF', "CPF to pay for loan", 50000),
               numericInput('LivingExpenses', "Living Expenses (Combined)", 1500),
               selectInput('HousingType', 'Housing Type', c(apartment_type, 'No Preference'), 'No Preference'),
               selectInput('Region', 'Region', c(region, 'All'),'All'),
               sliderInput("size", "Size of House (in square metres)", 30, 200, 30),
               sliderInput('age', 'House Age in years', 0, 99, 5),
               sliderInput('time', 'Loan Period in years', 10, 35, 10, step = 5),
               actionButton("element", "Submit"),
               actionButton("reset", "Reset")
)

map <- box(width = 3,
           height = DEFAULT_BOX_HEIGHT,
           background = 'orange',
           solidHeader = T,
           title = 'Map',
           leafletOutput('map')
)

table <- box(width = 7,
             height = DEFAULT_BOX_HEIGHT,
             title = 'Recommendations',
             DT::dataTableOutput('table')
)

body <- dashboardBody(filters, map, table)
header <- dashboardHeader(title = "Citi Technology Hackathon 2018", titleWidth = '20%')
sidebar <- dashboardSidebar(disable = T)
ui <- dashboardPage(header, sidebar, body)
