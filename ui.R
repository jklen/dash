
library(shiny) # load shiny at beginning at both scripts
library(plotly)
library(ggplot2)
library(dplyr)

shinyUI(pageWithSidebar( # standard shiny layout, controls on the
  # left, output on the right
  
  headerPanel("Utilization"), 
  sidebarPanel( 
    checkboxGroupInput(inputId = "center",
              label = "Center ID", 
              choices = c('LA' = 1,
                          'EMEA' = 2,
                          'AP' = 3,
                          'JP - Dalian' = 4)
    )
  ),
  
  mainPanel(
    plotOutput("plotDisplay")
  )
))