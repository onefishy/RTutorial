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
              tabPanel("Statistics Table",tableOutput(outputId="table"))
      )
    )
   )
  )

server <- function(input,output)  {
  
# name reactive values  
city <- reactive({
                  houses[which(houses$Location %in% input$loc &
                               houses$Status %in% input$status),] 
                })
ft <- reactive({lm(city()$Price~city()$Sqft)})
tab <- reactive({ 
                 median_cty <- median(city()$Price)
                 mean_cty   <- mean(city()$Price)
                 min_cty    <- min(city()$Price)
                 max_cty    <- max(city()$Price)
                 sd_cty     <- sd(city()$Price)
                 Housing_Price_Statistics   <- c(median_cty,mean_cty,
                                 min_cty,max_cty,
                                 sd_cty)
                 data.frame(Housing_Price_Statistics,row.names=c("median","mean",
                                                "min","max","sd")
                            ) 
               })

# compile output functions
  output$hist <- renderPlot({
                             hist(city()$Price,main="Histogram of Home Prices",
                                  xlab="Price",breaks=20)
                             box()
                 })
  output$line <- renderPlot({
                             plot(city()$Sqft,city()$Price,
                                  main="Scatterplot of Home Prices",
                                  type="p",col="blue",xlab="Sqft",
                                  ylab="Price"
                                  )
                             abline(ft())
                 })
  output$table <- renderTable(tab())                                 
  
}

shinyApp(ui=ui,server=server)