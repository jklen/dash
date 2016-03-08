
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

mqt_utilization <- read.csv('mqt_utilization.csv')

shinyServer(function(input, output) { # server is defined within
  # these parentheses
  
  output$plotDisplay <- renderPlot({
    hist(rnorm(input$obs), main = input$plot_name)
    
  })
  output$textDisplay <- renderText({
    paste0('Observations: ', input$obs)
  })
})

# aspon 3 metriky co monitoruju vykon nejakej role
# 
  # WPL - QA app rate
  #     - sponosor app rate
  #     - WDL app rate?
  #     - OTD to sponsor
  #     - turnaround?
  #     - waiting times?
  # WB  - report EM Web builder?
# par globalnych metrik
  # utilizacia
  # 
# analyticka funkcionalita - rozne metriky na task?
  # 1 premenna - grafy, summary
  # 2 a viac premennych
  # upload suboru
# vizualizacne vychytavky - rCharts, Google charts (google vis), plotly, ggvis, ggplot2
  # sankey diagram - budget a spending projektov?
  # interaktivna mapa, nejaka jednoducha metrika na staty

