#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    setwd('~/RTutorial/Data')
    mydata = read.csv("California_Home_Prices_(2009).csv")
    
    # generate bins based on input$bins from ui.R
    prices    <- subset(mydata, Location==input$loc)$Price
    bins <- seq(min(prices), max(prices), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    if (min(prices) != max(prices)){
      hist(prices, breaks = bins, 
          col = 'darkgray', 
          border = 'white')
    }
    else{
      hist(prices, breaks = 1, 
           col = 'darkgray', 
           border = 'white')
    }
    
  })
  
})
