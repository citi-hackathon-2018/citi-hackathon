sglng = 103.8198
sglat = 1.3521

map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = sglng,
             lat = sglat
  )


server <- function(input, output) {
  output$table = renderDataTable(data)
  
  ### Logic Layer
  
  
  ###when sumbit is pressed, filter all data
  #observeEvent(input$element, {
  #data1 = data
  #if(input$Location !='All){ data1 = data[which(data$location==input$Location),]}
  #data1 = data1[which(data1$var1 == input$var1),]
  #output$table = renderDataTable({data1})
  #})
  
  ## resetting recommendations
  observeEvent(input$reset,{output$table = renderDataTable({data})})
  output$mymap <- renderLeaflet(
    map
  )
}