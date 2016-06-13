library(shiny)

csvData = read.csv(file = "C:/Users/IrvPe/RTutorial/Data/California Home Prices (2009).csv")

# Subset all columns 
?subset()

str(csvData$Price)


{
subset(csvData, 
       subset = Location == "Nipomo" & Bedrooms == 3 & Price > 300000 & Price < 600000
       )
}

subset(csvData, 
       subset = csvData$Location, 
       select = names(csvData))



ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output){
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)