
library(shiny)
library(ggplot2)
library(dplyr)
library(ggExtra)
library(lubridate)


shinyUI(pageWithSidebar(
  
  headerPanel("Utilization"), 
  
  sidebarPanel(
    
    dateRangeInput(inputId = 'date_range',
                   label = 'Select date range',
                   weekstart = 1,
                   start = now() - months(13),
                   end = max(df_util$YEARMONTH),
                   min = min(df_util$YEARMONTH),
                   max = max(df_util$YEARMONTH)
      
    ),
    
    sliderInput(inputId = 'util_value',
                label = 'Utilization between',
                min = 0,
                max = ceiling(max(df_util$util_bill)),
                value = c(0, ceiling(max(df_util$util_bill))),
                round = -2,
                step = 0.01
    ),
    
    radioButtons(inputId = "grouping",
              label = "Select grouping", 
              choices = c('Geo' = 'GEO_NAME',
                          'Organization' = 'ORG_NAME',
                          'Department' = 'DEPT_NAME',
                          'User' = 'USER_NAME')
    ),
    
    div(style = 'height: 220px; overflow:scroll', uiOutput('reac_units')),
    
    checkboxInput(inputId = 'check_uplevel',
                  label = 'Visualize level above',
                  value = F
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
    
      tabPanel('Utilization', plotOutput("utilization_YM"),
               plotOutput('utilization_selected'),
               dataTableOutput('utilization_users')
      ),
      
      tabPanel('QA approval rate', plotOutput('QA approval rate')
      
      )
      
    )
  )
))

# filtre - date_range, util_value, reac_units -> grouping - grouping -> union - check_uplevel