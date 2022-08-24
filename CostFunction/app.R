#
# Objective: create a Shiny app to explore cost function subadditivity
# Author: Grant Coble-Neal
#
library(plotly)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Surface plot of multi-output cost function"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("params1",
                   "Output 1 parameter:",
                   min = -1,
                   max = 1,
                   value = 0.1),
      numericInput("params2",
                   "Output 2 parameter:",
                   min = -1,
                   max = 1,
                   value = 0.07),
      numericInput("params3",
                   "Fixed cost:",
                   min = 0,
                   max = 1000,
                   value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("SurfacePlot", height = 750, width = 750)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$SurfacePlot <- renderPlotly({
    
    x=y= seq(1,10,by = .1)
    TC = function(x,y){z=input$params1*x^2+input$params2*y^2+input$params3}
    z = outer(x,y,TC)
    
    
    plot_ly() %>% add_surface(x = x, y = y, z = z)  %>% 
      layout(title = 'Subadditive cost function in electricity distribution',
             scene = list(xaxis = list(title = 'Output 1'),
                          yaxis = list(title = 'Output 2'),
                          zaxis = list(title = 'Total cost')))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
