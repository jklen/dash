
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) { # server is defined within
  # these parentheses
  
  output$plotDisplay <- renderPlot({
    hist(rnorm(input$obs), main = input$plot_name)
    
  })
  output$textDisplay <- renderText({
    paste0('Observations: ', input$obs)
  })
})
