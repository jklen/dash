
library(shiny) # load shiny at beginning at both scripts
library(plotly)
library(ggplot2)
library(dplyr)

shinyUI(pageWithSidebar( # standard shiny layout, controls on the
  # left, output on the right
  
  headerPanel("Utilization"), 
  
  sidebarPanel( 
    
    radioButtons(inputId = "grouping",
              label = "Select grouping", 
              choices = c('Geo',
                          'Organization',
                          'Department',
                          'User')
    ),
    
    div(style = 'height: 220px; overflow:scroll', uiOutput('reac_units'))
  ),
  
  mainPanel(
    
    plotOutput("utilization")
  )
))