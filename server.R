library(shinydashboard)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(loggit)

setLogFile("logs/logs.json")
loggit("INFO", "app has started")

PROPERTY_DATA_COLUMN_NAMES = c('Region', 'Apartment Type', 'Block', 'Street Name', 'Floor', 
                               'Size', 'Price', 'Address', 'Age', 'Longitude', 'Latitude')
PROPERTY_COORDINATE_COLUMN_POSITIONS = c(10, 11)
PROPERTY_INFO_COLUMN_POSITIONS = c(1 : 9)
property_data = read.csv('data/properties.csv', header = T)
colnames(property_data) = PROPERTY_DATA_COLUMN_NAMES
coordinate_data = property_data[, PROPERTY_COORDINATE_COLUMN_POSITIONS]
property_info = property_data
apartment_type = levels(property_info$'Apartment Type')
region = levels(property_info$'Region')

# Misc Constants
MONTHS_IN_A_YEAR = 12
LOWER_ROOT_BOUND = 0
UPPER_ROOT_BOUND = 10^99

# UI Default Inputs
DEFAULT_INCOME = 3000
DEFAULT_CPF = 50000
DEFAULT_LIVING_EXPENSES = 1500
DEFAULT_HOUSING_TYPE = 'No Preference'
DEFAULT_REGION = 'All'
DEFAULT_SIZE = 30
DEFAULT_DIST = 200
DEFAULT_AGE = 5
DEFAULT_TIME = 10

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
DEFAULT_ZOOM = 11

# Record Fields
RECORD_FIELDS = c('MonthlyIncome','CPF','LivingExpenses','HousingType','Region',
                  "size",'age','time')

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
  monthlypayment = function(c){-pv+c/(ear/MONTHS_IN_A_YEAR)*(1-1/((1+(ear/MONTHS_IN_A_YEAR))^(MONTHS_IN_A_YEAR*time)))}
  alpha = uniroot(monthlypayment , lower = LOWER_ROOT_BOUND, upper = UPPER_ROOT_BOUND)$root
  return(alpha)
}
repayment_total = function(house_price, cpf, age){
  stamp = stamp_duty(house_price)
  cpf = cpf_available(cpf, age)
  mortgage = mortgage_fee(house_price)
  cpf_grant = grant(age)
  return(house_price + stamp - cpf + mortgage - cpf_grant)
}

# Map Object
map <- leaflet() %>%
       addTiles() %>%
       setView(zoom = DEFAULT_ZOOM,
               lng = SINGAPORE_LONGITUDE,
               lat = SINGAPORE_LATITUDE)



server <- function(input, output, session) {
  observeEvent(input$plot_input, {
    user_data = read.csv('data/user_data.csv',header = T)[,-1]
    region = user_data$Region
    if (input$plot_input == 'Region') {
      output$plot = renderPlot({
        barplot(table(region), ylab = 'Number of Searches', xlab = 'Regions searched',yaxt='n')
        axis(2, col.axis="black", las=2)
      })
    } else {
      output$plot = renderPlot({
        barplot(table(repay_level), ylab = 'Number of Searches', xlab = 'Average Total Loan',yaxt='n')
        axis(2, col.axis="black", las=2)})
    }
  }
  )

  output$table = renderDataTable(property_data[, 1:8])
  output$map <- renderLeaflet(map)
  
  ### When sumbit is pressed, filter all data
  observeEvent(input$element, {
    loggit("INFO", "user has submitted something")
    if (input$Region !='All'){ property_info = property_info[which(property_info$Region==input$Region),]}
    if (input$HousingType != 'No Preference'){property_info = property_info[which(property_info$'Apartment Type' == input$HousingType),]}
    property_info = property_info[which(property_info$Size >= input$size),]
    property_info = property_info[which(property_info$Age<= input$age),]
    
    repay = NULL
    if (nrow(property_info)!=0){
      repay = repayment_total(property_info$Price,input$CPF,property_info$Age)
      repay_vec = numeric (length(repay))
      for (i in 1:length(repay)){
        repay_vec[i]= round(repayment_monthly(repay[i],0.035,input$time),2)
      }
      vector = which(repay_vec<=(0.7*input$MonthlyIncome)-input$LivingExpenses)
      affordable_h = property_info[vector,]
      output$map = renderLeaflet(map %>%
                                   addMarkers(lat = property_info$Latitude,
                                              lng = property_info$Longitude))
      filtered = property_info[, c(1,2,5,6,7,8,9,10)]
      if(nrow(affordable_h)!=0){filtered = cbind(affordable_h, data.frame(repay_vec[vector]))}
      colnames(filtered)[ncol(filtered)] = 'Repayment per Month'
      output$table = renderDataTable({filtered})
      
    }
    
    ## Converting all inputs into Dataframe ##
    form_data = reactive ({
      data = sapply(RECORD_FIELDS, function(x) {input [[x]]})
      data
    })
    
    ## Saving Data when Submit is pressed ##
    save_data = function(data){
      data = as.data.frame(t(data), stringsAsFactors=F)
      datafile = read.csv('data/user_data.csv', header = T)
      written = rbind(datafile[-1], data)
      
      write.csv(x=written, 'data/user_data.csv', row.names=T, quote = T)}
    
    save_data(form_data())
    
  })
  
  ## resetting recommendations
  observeEvent(input$reset,{
    output$table = renderDataTable({property_data[, 1:8]})
    updateNumericInput(session, 'MonthlyIncome', value = 3000 )
    updateNumericInput(session, 'CPF', value = 50000 )
    updateNumericInput(session, 'LivingExpenses', value = 1500)
    updateSelectInput(session, 'HousingType', selected = 'No Preference' )
    updateSelectInput(session, 'Region', selected = 'All')
    updateSliderInput(session, 'size', value = min(property_data$Size))
    updateSliderInput(session, 'dist', value = 200)
    updateSliderInput(session, 'age', value = 5)
    updateSliderInput(session, 'time', value = 10)
    output$map = renderLeaflet({map <- leaflet() %>%
                                addTiles() %>%
                                setView(zoom = DEFAULT_ZOOM,
                                        lng = SINGAPORE_LONGITUDE,
                                        lat = SINGAPORE_LATITUDE)})
  })
  
  onStop(function() {loggit("INFO", "app has stopped")})
}