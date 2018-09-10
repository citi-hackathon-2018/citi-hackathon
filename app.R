library(shinydashboard)
library(shiny)
library(leaflet)

### Misc
sglng = 103.8198
sglat = 1.3521

### Mock Data
data = read.csv('data/properties.csv', header = T)
data = cbind(data,rep(30,nrow(data)))
colnames(data) = c('Region','Flat Type','Block','Street Name','Story Range','Size', 'Price','Address','Longitude','Latitude','Age')
coord_data = data[c(9,10)]
apartment_type = levels(data$'Flat Type')
region = levels(data$'Region')
data1 = data [,c(1:8,11)]



### Map Object
map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = sglng,
             lat = sglat
  )

### Mortgage fees and other housing related misc
legalfees = 2750
valuationfees = 350
mortgage_fee = function(houseprice) {return(min(500,0.004*houseprice))}

grant = function(age){
  ifelse(age <=70, 20000,0)
}

stampduty = function(houseprice){
  ifelse(houseprice <= 180000, 0.01*houseprice,ifelse(houseprice <= 360000,0.01*180000+0.02*(houseprice - 180000),ifelse(houseprice<=1000000,0.03*180000+(0.03*(houseprice-360000)),0.03*180000+0.03*640000+0.04*(houseprice - 1000000))))
}

repayment_monthly = function(pv,ear,time){
  monthlypayment = function(c){-pv+c/(ear/12)*(1-1/((1+(ear/12))^(12*time)))}
  alpha = uniroot(monthlypayment,lower = 0, upper = 10^99)$root
  return(alpha)
}

repayment_total = function(houseprice,cpf,age){
  stamp_duty = stampduty(houseprice)
  cpf = ifelse(age<=69, cpf, 0)
  m_fee = mortgage_fee(houseprice)
  cpfgrant = grant(age)
  return (houseprice + stamp_duty-cpf+m_fee-cpfgrant)
}


ui <- dashboardPage(
  dashboardHeader(title = "Citi Technology Hackathon 2018", titleWidth = 450),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(column(width = 12,
      box(width = 4,background = 'maroon', solidHeader = T,
        title = "Filters",
        numericInput('MonthlyIncome',"Monthly Income (Combined)",3000),
        numericInput('CPF',"CPF to pay for loan",50000),
        numericInput('LivingExpenses',"Living Expenses (Combined)",1500),
        selectInput('HousingType', 'Housing Type', c(apartment_type,'No Preference'),'No Preference'),
        selectInput('Region','Region', c(region,'All'),'All'),
        sliderInput("size", "Size of House (in square metres)", min(data$Size), max(data$Size), min(data$Size)),
        #sliderInput('dist','Distance from MRT (in metres)',200,1000,200),
        sliderInput('age','Housing Age',0,99,5),
        sliderInput('time','Loan Period',10,35,10, step = 5),
        actionButton("element","Submit"),
        actionButton("reset","Reset")
      ),
      
      box(width = 8, background = 'orange', solidHeader = T,
        title = 'Maps',
        leafletOutput("mymap",height = 700),
        p()
      )
    ),
      column(width = 12,
      box (height = NULL,
           title = 'Recommendations',
           DT::dataTableOutput('table')
      )
    ))
))
server <- function(input, output,session) {
  
  output$table = renderDataTable(data[,1:8])
  output$mymap <- renderLeaflet(map)
  
  
  ###when sumbit is pressed, filter all data
  observeEvent(input$element, {
  if(input$Region !='All'){ data1 = data1[which(data1$Region==input$Region),]}
  if (input$HousingType != 'No Preference'){data1 = data1[which(data1$'Flat Type' == input$HousingType),]}
  data1 = data1[which(data1$Size >= input$size),]
  data1 = data1[which(data1$Age<= input$age),]
  if (nrow(data1)!=0){
  repay = repayment_total(data1$Price,input$CPF,data1$Age)
  repay_vec = numeric (length(repay))
  for (i in 1:length(repay)){
    repay_vec[i]= round(repayment_monthly(repay[i],0.035,input$time),2)
  }
  vector = which(repay_vec<=0.7*(input$MonthlyIncome-input$LivingExpenses))
  affordable_h = data1[vector,]
  if(nrow(affordable_h)!=0){data1 = cbind(affordable_h,data.frame(repay_vec[vector]))}
  colnames(data1)[ncol(data1)]= 'Repayment per Month'
  output$mymap = renderLeaflet(leaflet() %>%addTiles() %>%
                                 addMarkers(lat = coord_data$Latitude[vector],lng = coord_data$Longitude[vector]))
  }
  output$table = renderDataTable({data1})
  })
  
  ## resetting recommendations
  observeEvent(input$reset,{
    output$table = renderDataTable({data[,1:8]})
    updateNumericInput(session,'MonthlyIncome',value = 3000 )
    updateNumericInput(session,'CPF',value = 50000 )
    updateNumericInput(session,'LivingExpenses',value = 1500)
    updateSelectInput(session,'HousingType',selected = 'No Preference' )
    updateSelectInput(session,'Region',selected = 'All')
    updateSliderInput (session, 'size',value = min(data$Size))
    updateSliderInput (session, 'dist',value = 200)
    updateSliderInput (session, 'age',value = 5)
    updateSliderInput (session, 'time',value = 10)
    output$mymap = renderLeaflet(map)
  })
  
  ###Map Checkbox
  #observe({ if(length (input$Location)> 1) {updateCheckboxGroupInput(session,"Location", selected= tail(input$Location,1))}})
}

shinyApp(ui=ui, server=server)
