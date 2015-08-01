library(shiny)

shinyServer(function(input, output,session) {
  source('yelpfunctions.R')
  initializeData()
  library(wordcloud)
  library(RColorBrewer)
  finalData <- data.frame
  # Return the requested dataset
  
  datasetInput <- reactive({
      print(input$bussnm)
      finalData <- process(input$bussnm)
      finalData
  })
    
  # Show the first "n" observations
  output$view <- renderTable({
    datasetInput()[[1]]
  })
  
  output$tipview <- renderTable({
    datasetInput()[[3]]
  })
  
  output$bussview <- renderTable({
    datasetInput()[[2]]
  })
  
  output$myImage <- renderImage({
    # Return a list containing the filename
    
    getImage(datasetInput()[[1]])
    
    list(src = "one.png",
         contentType = 'image/png',
         width = 600,
         height = 300)
  })
})