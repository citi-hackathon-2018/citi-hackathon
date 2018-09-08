library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)

### Misc

rownames= c('Monthly Income','CPF to pay for Loan','Living Expenses','Housing Type','Location','Size')
data1 = 0

legalfees = 2750
valuationfees = 350
mortgage_fee = function(houseprice) {return(min(500,0.004*houseprice))}
grant = function(age,type){
  ifelse(age <=70 && type == 'HDB', 20000,0)
}

stampduty = function(houseprice){
  ifelse(houseprice <= 180000, 0.01*houseprice,ifelse(houseprice <= 360000,0.01*180000+0.02*(houseprice - 180000),ifelse(houseprice<=1000000,0.03*180000+(0.03*(houseprice-360000)),0.03*180000+0.03*640000+0.04*(houseprice - 1000000))))
}

repayment_monthly = function(pv,ear,time){
  monthlypayment = function(c){
    -pv + c/ear*(1-1/((1+ear)^time))
  }
  alpha = uniroot(monthlypayment,lower = 0, upper = 10^99)$root
  return(alpha)
}

repayment_total = function(houseprice,cpf,loan,age,type){
  stamp_duty = stampduty(houseprice)
  cpf = ifelse(age<=69, cpf, 0)
  m_fee = mortgage_fee(houseprice)
  cpfgrant = grant(age,type)
  return (houseprice - stamp_duty-cpf-m_fee+cpfgrant)
}
#data = read.csv ()
#housingtype = data$housingtype
#region = data$region

ui <- dashboardPage(
  dashboardHeader(title = "Citi Technology Hackathon 2018", titleWidth = 450),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(column(width = 12,
                    box(width = 3,background = 'maroon', solidHeader = T,
                        title = "Filters",
                        numericInput('MonthlyIncome',"Monthly Income",3000),
                        numericInput('CPF',"CPF to pay for loan",50000),
                        numericInput('LivingExpenses',"Living Expenses",1500),
                        selectInput('HousingType', 'Housing Type', c('1 room HDB','2 room HDB','3 room HDB','4 room HDB', '5 room HDB',
                                                                     'Condominium','Pan House','Terrace','Semi-Detached','Bungalow'),'1 Room HDB'),
                        selectInput('Location','Region', c('North','South','East','West','Central','All'),'All'),
                        sliderInput("size", "Size of House (in square metres)", 36, 500, 36),
                        actionButton("element","Submit"),
                        actionButton("reset","Reset")
                    ),
                    
                    box(width = 9, background = 'orange', solidHeader = T,
                        title = 'Maps',
                        leafletOutput("mymap",height = 350),
                        p(),
                        checkboxGroupInput('Location','Location', c('North', 'South', 'East','West','Central'))
                    )
    ),
    column(width = 12,
           box (height = NULL,
                title = 'Recommendations',
                DT::dataTableOutput('table')
           )
    ))
  ))