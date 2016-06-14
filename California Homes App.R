houses <- read.csv("California Home Prices (2009).csv")
library(shiny)
ui <- fluidPage(
  sidebarLayout(
    
    
    sidebarPanel(
      # location select thingy
      selectInput(inputId="loc",
                  label="Location of Houses",
                  choices=levels(houses$Location),
                  multiple=TRUE,selected="Arroyo Grande"
      ),
      # status select thingy
      selectInput(inputId="status",
                  label="Status of House at Sale Time",
                  choices=levels(houses$Status),multiple=TRUE,
                  selected="Regular"
      )
    ),
    
    # output tabs
    mainPanel(
      tabsetPanel(type="tabs",
              tabPanel("Histogram",plotOutput(outputId="hist")),
              tabPanel("Line Graph",plotOutput(outputId="line")),
              tabPanel("Statistics Table",plotOutput(outputId="table"))
      )
    )
   )
  )

server <- function(input,output)  {
  
# name reactive values  
city <- reactive({subset(houses, Location==input$loc&Status==input$status)
                })
ft <- reactive({lm(city()$Price~city()$Sqft)})
tab <- reactive({ as.data.frame( summary(city()$Price) ) })

# compile output functions
  output$hist <- renderPlot({
                             hist(city()$Price,main="Histogram of Home Prices",
                                  xlab="Price")
                 })
  output$line <- renderPlot({
                             plot(city()$Sqft,city()$Price,
                                  main="Scatterplot of Home Prices",
                                  type="p",col="blue",xlab="Sqft",
                                  ylab="Price"
                                  )
                             abline(ft())
                 })
  output$table <- renderDataTable({ tab() })
  
}

shinyApp(ui=ui,server=server)