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
      
      sliderInput(inputId = "price",
                  label   = "Price",
                  min     = 1,
                  max     = max(mydata$Price),
                  value   = sample(mydata$Price, 1)), # returns 1 random element of mydata$price
      
      numericInput( inputId = "beds",
                    label   = "# of bedrooms",
                    min     = min(mydata$Bedrooms),
                    max     = max(mydata$Bedrooms),
                    value   = sample(mydata$Bedrooms, 1)),
      
      numericInput( inputId = "baths",
                    label   = "# of bathrooms",
                    min     = min(mydata$Bathrooms, na.rm = TRUE),
                    max     = max(mydata$Bathrooms, na.rm = TRUE),
                    value   = sample(mydata$Bathrooms, 1)),
      
      sliderInput( inputId = "sqft",
                   label   = "Sq. ft.",
                   min     = min(mydata$Sqft, na.rm = TRUE),
                   max     = max(mydata$Sqft, na.rm = TRUE),
                   value   = sample(mydata$Sqft, 1)),
      
      sliderInput( inputId = "pp_sq_ft",
                   label   = "Sq. ft.",
                   min     = min(mydata$Price.Sqft, na.rm = TRUE),
                   max     = max(mydata$Price.Sqft, na.rm = TRUE),
                   value   = sample(mydata$Price.Sqft, 1)),
      
      selectInput( inputId = "sale_type",
                   label   = "Sale Type",
                   choices = levels(mydata$Status)),
    
    
      selectInput( inputId   = "cities",
                 label     = "Location",
                 choices   = levels(mydata$Location),
                 multiple  = FALSE,
                 selectize = FALSE,
                 size      = length(levels(mydata$Location)))
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
  
  city       = reactive({input$cities})
  max_price  = reactive({input$price})
  sale_type  = reactive({input$sale_type})
  sqft       = reactive({input$sqft})
  baths      = reactive({input$baths})
  beds       = reactive({input$beds})
  price_sqft = reactive({input$pp_sq_ft})
  
  # I'd like to get this block to work, using subset() in order
  #  to subset the data. I've gone onto the %in% syntax for now.

  # relevant_data <- reactive({
  #   subset(mydata,
  #          subset =
  #            Location   == city() &
  #            Bedrooms   == beds() &
  #            Bathrooms  == baths() &
  #            Sqft       == sqft() &
  #            Status     == sale_type() )
  # })
  
  
  # Should do the same as the above block, but this uses th %in% syntax
  relevant_data <- reactive({
    mydata[which(
           mydata$Location %in% input$cities &
           mydata$Bedrooms %in% input$beds &
           mydata$Bathrooms %in% input$baths&
           mydata$Sqft %in% input$sqft &
           mydata$Status %in% input$sale_type
          ), ]
  })
  
  # Histogram code
  output$histPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    prices    <- relevant_data$Price
    bins <- seq(min(prices), max(prices), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(prices, breaks = 50, col = 'darkgray', border = 'white')
  })
  
  output$scatterPlot <- renderPlot({
    #scatter plot
    plot(relevant_data()$Sqft, relevant_data()$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
         xlab="Sqft", ylab="Price", pch=19)
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)