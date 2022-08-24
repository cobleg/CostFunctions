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
                   value = 100),
      numericInput("height",
                   "Flat plane height:",
                   min = 0,
                   max = 1000,
                   value = 95),
      numericInput("slope_x",
                   "Flat plane x-slope:",
                   min = 0,
                   max = 10,
                   value = 0.95),
      numericInput("slope_y",
                   "Flat plane y-slope:",
                   min = 0,
                   max = 10,
                   value = 0.85)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("SurfacePlot", height = 750, width = 750),
      HTML(paste("A function is subadditive if the output of a function evaluated at a given level of input is than 
                 or equal to the sum of the function output evaluated separately for any subdivided input, i.e.")),
      uiOutput("subadditive"),
      HTML(paste("A subadditive cost function implies that for a given level of output, it is less costly to employ a single production process
                 to supply all of the output than separate production processes that supply that output. 
                 For example, it is typically less costly for a single pipeline to transport gas from point A to point B than two or more separate pipelines."))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$SurfacePlot <- renderPlotly({
    
    x=y= seq(1,10,by = .1)
    TC = function(x,y){z=input$params1*x^2+input$params2*y^2+input$params3}
    z = outer(x,y,TC)
    line = function(x,y){z=input$slope_x*x+input$slope_y*y+input$height}
    z2 = outer(x,y,line)
    
    plot_ly() %>% add_surface(x = x, y = y, z = z)  %>% 
      add_surface(x = x, y = y, z = z2, name = "slope of 10") %>%
      layout(title = 'Subadditive cost function',
             scene = list(xaxis = list(title = 'Output 1'),
                          yaxis = list(title = 'Output 2'),
                          zaxis = list(title = 'Total cost')))
  })
  
  output$subadditive <- renderUI({
    withMathJax(
      helpText('$$ f(x+y) \\le f(x) + f(y) $$')
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
