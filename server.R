
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# shinyServer(function(input, output) {
# 
#   output$distPlot <- renderPlot({
# 
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#   })
# 
# })

library(shiny)

shinyServer(function(input, output) { # server is defined within
  # these parentheses
  
  output$plotDisplay <- renderPlot({
    hist(rnorm(input$obs), main = input$plot_name)
    
  })
  output$textDisplay <- renderText({
    paste0('Observations: ', input$obs)
  })
})
