library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)

PROPERTY_DATA_COLUMN_NAMES = c('Region', 'Apartment Type', 'Block', 'Street Name', 'Story Range', 
                               'Size', 'Price', 'Address', 'Age', 'Longitude', 'Latitude')
PROPERTY_COORDINATE_COLUMN_POSITIONS = c(10, 11)
PROPERTY_INFO_COLUMN_POSITIONS = c(1 : 9)

property_data = read.csv('data/properties.csv', header = T)
colnames(property_data) = PROPERTY_DATA_COLUMN_NAMES
coordinate_data = property_data[, PROPERTY_COORDINATE_COLUMN_POSITIONS]
property_info = property_data[, PROPERTY_INFO_COLUMN_POSITIONS]
apartment_type = levels(property_info$'Apartment Type')
region = levels(property_info$'Region')

# Fee-related Constants
LEGAL_FEES = 2750
VALUATION_FEES = 350
MINIMUM_MORTGAGE_FEE = 500
MORTAGE_RATE = 0.004
GRANT_AMOUNT = 20000
MAXIMUM_GRANT_AGE = 70
STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_1 = 180000
STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_2 = 360000
STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_3 = 1000000
STAMP_DUTY_RATE_CATEGORY_1 = 0.01
STAMP_DUTY_RATE_CATEGORY_2 = 0.02
STAMP_DUTY_RATE_CATEGORY_3 = 0.03
STAMP_DUTY_RATE_CATEGORY_4 = 0.04
MAXIMUM_CPF_AGE = 69

# Map-related Constants
SINGAPORE_LONGITUDE = 103.8198
SINGAPORE_LATITUDE = 1.3521

mortgage_fee = function(house_price) {return(min(MINIMUM_MORTGAGE_FEE, MORTAGE_RATE * house_price))}
cpf_available = function(cpf, age) {return(ifelse(age <= MAXIMUM_CPF_AGE, cpf, 0))}

eligible_for_grant = function(age) {return(age <= MAXIMUM_GRANT_AGE)}
grant = function(age) {return(ifelse(eligible_for_grant(age), GRANT_AMOUNT, 0))}

stamp_duty_category_1 = function(house_price) {return(STAMP_DUTY_RATE_CATEGORY_1 * house_price)}
stamp_duty_category_2 = function(house_price) {
  applicable_on_first_cat = STAMP_DUTY_RATE_CATEGORY_1 * STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_1
  applicable_on_second_cat = STAMP_DUTY_RATE_CATEGORY_2 * (house_price - STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_2)
  return(applicable_on_first_cat + applicable_on_second_cat)
}
stamp_duty_category_3 = function(house_price) {
  applicable_on_first_cat = STAMP_DUTY_RATE_CATEGORY_3 * STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_1
  applicable_on_second_cat = STAMP_DUTY_RATE_CATEGORY_3 * (house_price - STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_2)
  return(applicable_on_first_cat + applicable_on_second_cat)
}
stamp_duty_category_4 = function(house_price) {
  applicable_on_first_cat = STAMP_DUTY_RATE_CATEGORY_3 * STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_1
  applicable_on_second_cat = STAMP_DUTY_RATE_CATEGORY_3 * 
    (STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_3 - STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_2)
  applicable_on_third_cat = STAMP_DUTY_RATE_CATEGORY_4 * (house_price - STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_3)
  return(applicable_on_first_cat + applicable_on_second_cat + applicable_on_third_cat)
}
stamp_duty = function(house_price){
  ifelse(house_price <= STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_1,
         stamp_duty_category_1(house_price),
         ifelse(house_price <= STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_2,
                stamp_duty_category_2(house_price),
                ifelse(house_price <= STAMP_DUTY_HOUSE_PRICE_APPLICABLE_CATEGORY_3,
                       stamp_duty_category_3(house_price), 
                       stamp_duty_category_4(house_price))))
}

repayment_monthly = function(pv,ear,time){
  monthlypayment = function(c){-pv+c/(ear/12)*(1-1/((1+(ear/12))^(12*time)))}
  alpha = uniroot(monthlypayment,lower = 0, upper = 10^99)$root
  return(alpha)
}

repayment_total = function(house_price, cpf, age){
  stamp = stamp_duty(house_price)
  cpf = cpf_available(cpf, age)
  mortgage = mortgage_fee(house_price)
  cpf_grant = grant(age)
  return(house_price + stamp - cpf + mortgage - cpf_grant)
}

# Map-related Constants
SINGAPORE_LONGITUDE = 103.8198
SINGAPORE_LATITUDE = 1.3521
DEFAULT_ZOOM = 11

# Map Object
map <- leaflet() %>%
       addTiles() %>%
       setView(zoom = DEFAULT_ZOOM,
               lng = SINGAPORE_LONGITUDE,
               lat = SINGAPORE_LATITUDE)

server <- function(input, output, session) {
  
  output$table = renderDataTable(property_data[, 1:8])
  output$map <- renderLeaflet(map)
  
  ###when sumbit is pressed, filter all data
  observeEvent(input$element, {
    if(input$Region !='All'){ property_info = property_info[which(property_info$Region==input$Region),]}
    if (input$HousingType != 'No Preference'){property_info = property_info[which(property_info$'Flat Type' == input$HousingType),]}
    property_info = property_info[which(property_info$Size >= input$size),]
    property_info = property_info[which(property_info$Age<= input$age),]
    if (nrow(property_info)!=0){
      repay = repayment_total(property_info$Price,input$CPF,property_info$Age)
      repay_vec = numeric (length(repay))
      for (i in 1:length(repay)){
        repay_vec[i]= round(repayment_monthly(repay[i],0.035,input$time),2)
      }
      vector = which(repay_vec<=0.7*(input$MonthlyIncome-input$LivingExpenses))
      affordable_h = property_info[vector,]
      if(nrow(affordable_h)!=0){property_info = cbind(affordable_h,data.frame(repay_vec[vector]))}
      colnames(property_info)[ncol(property_info)]= 'Repayment per Month'
      output$map = renderLeaflet(map %>%
                                 addMarkers(lat = coordinate_data$Latitude[vector],
                                            lng = coordinate_data$Longitude[vector]))
    }
    output$table = renderDataTable({property_info})
  })
  
  ## resetting recommendations
  observeEvent(input$reset,{
    output$table = renderDataTable({data[, 1:8]})
    updateNumericInput(session, 'MonthlyIncome', value = 3000 )
    updateNumericInput(session, 'CPF', value = 50000 )
    updateNumericInput(session, 'LivingExpenses', value = 1500)
    updateSelectInput(session, 'HousingType', selected = 'No Preference' )
    updateSelectInput(session, 'Region', selected = 'All')
    updateSliderInput(session, 'size', value = min(data$Size))
    updateSliderInput(session, 'dist', value = 200)
    updateSliderInput(session, 'age', value = 5)
    updateSliderInput(session, 'time', value = 10)
    output$map = renderLeaflet({map <- leaflet() %>%
                                addTiles() %>%
                                setView(zoom = DEFAULT_ZOOM,
                                        lng = SINGAPORE_LONGITUDE,
                                        lat = SINGAPORE_LATITUDE)})
  })
}