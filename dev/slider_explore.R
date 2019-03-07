library(shiny)
library(shinyWidgets)

# With Modern design

ui <- fluidPage(
  chooseSliderSkin("HTML5"),
  sliderInput("obs", "Customized single slider:",
              min = 0, max = 100, value = 50
  ),
  sliderInput("obs2", "Customized range slider:",
              min = 0, max = 100, value = c(40, 80)
  ),
  plotOutput("distPlot")
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
  
}

shinyApp(ui, server)



# Use Flat design & a custom color

ui <- fluidPage(
  chooseSliderSkin("Square", color = "#112446"),
  sliderInput("obs", "Customized single slider:",
              min = 0, max = 100, value = 50
  ),
  sliderInput("obs2", "Customized range slider:",
              min = 0, max = 100, value = c(40, 80)
  ),
  sliderInput("obs3", "An other slider:",
              min = 0, max = 100, value = 50
  ),
  plotOutput("distPlot")
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui, server)


