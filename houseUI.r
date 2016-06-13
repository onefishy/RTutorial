#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

mydata = read.csv("California_Home_Prices_(2009).csv")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cali Housing Prices with Tabs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput( inputId = "cities",
                   label = "Location",
                   choices = levels(mydata$Location),
                   multiple = FALSE,
                   selectize = FALSE,
                   size = length(levels(mydata$Location))
                   )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Histogram", plotOutput("histPlot")), 
                  tabPanel("Scatter Plot", plotOutput("scatterPlot"))
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  #setwd('~/RTutorial/Data')
  #mydata = read.csv("California_Home_Prices_(2009).csv")
  
  output$histPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    prices    <- reactive({subset(mydata,
                        subset = Location == input$cities,
                        select = Price) })
    bins <- reactive({seq(min(prices), max(prices), length.out = input$bins + 1)})
    
    # draw the histogram with the specified number of bins
    hist(prices, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
##  output$scatterPlot <- renderPlot({
##    #scatter plot
##    plot(mydata$Sqft, mydata$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
##         xlab="Sqft", ylab="Price", pch=19)
##    
##  })
})

# Run the application 
shinyApp(ui = ui, server = server)