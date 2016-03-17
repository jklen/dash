
library(shiny)
library(ggplot2)
library(dplyr)
library(ggExtra)
library(lubridate)


shinyUI(fluidPage(
  
  titlePanel("Utilization"),
  
  sidebarPanel(
    
    dateRangeInput(inputId = 'date_range',
                   label = 'Select date range',
                   weekstart = 0,
                   start = now() - months(7),
                   end = max(df_util$YEARMONTH),
                   min = min(df_util$YEARMONTH),
                   max = max(df_util$YEARMONTH),
                   startview = 'year'
      
    ),
    
    sliderInput(inputId = 'util_value',
                label = 'Utilization between',
                min = 0,
                max = ceiling(max(df_util$util_bill)),
                value = c(0.01, ceiling(max(df_util$util_bill))),
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
    
    tabsetPanel(id = 'tabs_1',
    
      tabPanel('Utilization',
               
              fluidRow(column(10, plotOutput("utilization_YM", 
                            click = clickOpts(id="util_YM_click"))),
                       column(2, plotOutput('Utilization_marginal',
                            click = clickOpts(id = 'util_marginal_click')))
              ),
              
              fluidRow(
                verbatimTextOutput('clicked'),
                plotOutput('utilization_selected'),
                
                
                dataTableOutput('utilization_users')
                
              )
      
               
      ),
      
      tabPanel('QA approval rate', plotOutput('QA approval rate')
      
      )
      
    )
  )
))

# 
# tabsetPanel(id = 'tabs1',
#             tabPanel("main",
#                      fluidRow(
#                        column(8,
#                               plotOutput('plot1')),
#                        column(4,
#                               p('2nd column'))),
#                      fluidRow(
#                        p("2nd row of viewing area"))
#             ),
#             
#             tabPanel("second",
#                      p("main viewing area")),
#             tabPanel("third",
#                      p('main viewing area')
# filtre - date_range, util_value, reac_units -> grouping - grouping -> union - check_uplevel