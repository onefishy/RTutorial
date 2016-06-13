#Filtering data and getting summary statistics
library(shiny)

ui <- fluidPage(
  
  titlePanel("Housing Information"),
  
  sidebarLayout(
    
    sidebarPanel(
  
      #Input() functions
      
#      checkboxGroupInput(inputId ="location", label = h3("Select locations"), #Location Input
#                         choices = locationOpt, 
#                         selected = locationOpt),
      
      selectInput(inputId = "location", label =  h3("Select locations"), choices = locationOpt, 
                  selected = locationOpt, multiple = TRUE, size = length(locationOpt), selectize = FALSE),
      
      checkboxGroupInput(inputId ="status", label = h3("Select a status"), #Status Input
                         choices = saleOpt, 
                         selected = saleOpt),
      
      sliderInput(inputId = "priceRange", label = h3("Price Range"), #Price input
                  min = 26500, max = 5499000, value = c(26500, 5499000)),
      
      sliderInput(inputId = "bedRange", label = h3("Range for Number of Bedrooms"), #Bed input
                  min = 0, max = 10, value = c(0, 10)),
      
      sliderInput(inputId = "bathRange", label = h3("Range for Number of Bathrooms"), #Bath input
                  min = 1, max = 11, value = c(1, 11)),
      
      sliderInput(inputId = "sqftRange", label = h3("Range for Square Footage"), #Square Footage input
                  min = 120, max = 6800, value = c(120, 6800)),
      
      sliderInput(inputId = "pricesqRange", label = h3("Range for Price per Square Foot"), #Price/sqft input
                  min = 19.3, max = 1145, value = c(19.3, 1145)),
      
      selectInput(inputId ="depVar", label = h3("Select a dependent variable for regression"), 
                  choices = quantVariables, 
                  selected = "Price"),
      
      selectInput(inputId = "indVar", label = h3("Select an independent variable for regression"), 
                  choices = quantVariables, 
                  selected = "Bedrooms")
    
    ),
    
    mainPanel(
      #Output() functions
      verbatimTextOutput("stats"),
      verbatimTextOutput("std"),
      plotOutput("hist"),
      plotOutput("graph"),
      verbatimTextOutput("regStats")
    )
    
  )
  
)

server <- function(input, output) {
  
  #Load in data, list of locations/sale options
  houses <- read.csv("California Home Prices (2009).csv")
  locationOpt <- levels(houses$Location) #List of Locations
  saleOpt <- levels(houses$Status) #List of sale types
  quantVariables = list("Price" = "Price", 
                        "Number of Bedrooms" = "Bedrooms",
                        "Number of Bathrooms" = "Bathrooms",
                        "Square Footage" = "Sqft",
                        "Price per Square Foot" = "Price.Sqft")
  
  data <- reactive({ #Subset the data
    houses[which(houses$Location %in% input$location &
                   houses$Status %in% input$status &
                   houses$Price >= input$priceRange[1] & houses$Price <= input$priceRange[2] &
                   houses$Bedrooms >= input$bedRange[1] & houses$Bedrooms <= input$bedRange[2] &
                   houses$Bathrooms >= input$bathRange[1] & houses$Bathrooms <= input$bathRange[2] &
                   houses$Sqft >= input$sqftRange[1] & houses$Sqft <= input$sqftRange[2] &
                   houses$Price.Sqft >= input$pricesqRange[1] & houses$Price.Sqft <= input$pricesqRange[2]), 
           ]
  })
  
  fit <- reactive({ #Regression model
    lm(data()[,input$depVar] ~ data()[,input$indVar])
  })

  output$stats <- renderPrint({
    summary(data())
  })
  
  output$std <- renderPrint({ print("Standard Deviations")
    apply(na.omit(data()[,c("Price", "Bedrooms", "Bathrooms", "Sqft", "Price.Sqft")]), MARGIN = 2, FUN = sd)
  })
  
  
  output$hist <- renderPlot({
    hist(data()$Price, main = "Histogram of Home Prices", xlab = "Price", ylab = "Number of Houses")
  })
  
  
  output$graph <- renderPlot({ #Regression Scatterplot
    plot(x= data()[,input$indVar], y = data()[,input$depVar], main = "Regression", 
         xlab = names(quantVariables[which(quantVariables == input$indVar)]),
         ylab = names(quantVariables[which(quantVariables == input$depVar)]))
    abline(fit())
  })
  
  output$regStats <- renderPrint({ #Regression line
    summary(fit())
  })
}

shinyApp(ui = ui, server = server)