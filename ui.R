library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)

default_box_height = NULL
data = read.csv('data/properties.csv', header = T)
data = cbind(data,rep(30,nrow(data)))
apartment_type = levels(data$'Flat Type')
region = levels(data$'Region')

filters <- box(width = 2,
               height = default_box_height,
               background = 'maroon',
               solidHeader = T,
               title = "Filters",
               numericInput('MonthlyIncome',"Monthly Income (Combined)",3000),
               numericInput('CPF',"CPF to pay for loan",50000),
               numericInput('LivingExpenses',"Living Expenses (Combined)",1500),
               selectInput('HousingType', 'Housing Type', c(apartment_type,'No Preference'),'No Preference'),
               selectInput('Region','Region', c(region,'All'),'All'),
               sliderInput("size", "Size of House (in square metres)", min(data$Size), max(data$Size), min(data$Size)),
               sliderInput('age','Housing Age',0,99,5),
               sliderInput('time','Loan Period',10,35,10, step = 5),
               actionButton("element","Submit"),
               actionButton("reset","Reset")
)

map <- box(width = 3,
           height = default_box_height,
           background = 'orange',
           solidHeader = T,
           title = 'Map',
           leafletOutput('map')
)

table <- box(width = 7,
             height = default_box_height,
             title = 'Recommendations',
             DT::dataTableOutput('table')
)

body <- dashboardBody(filters, map, table)
header <- dashboardHeader(title = "Citi Technology Hackathon 2018",
                          titleWidth = '20%')
sidebar <- dashboardSidebar(disable = T)
ui <- dashboardPage(header, sidebar, body)