
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
    
    selectInput(inputId = 'marginal_vis',
                  label = 'Marginal - categories as:',
                  choices = c('Boxplots', 'Histograms'),
                  multiple = F,
                  selected = 'Boxplots'
    ),
    
    uiOutput('marg_cat_hist_type')
  ),
  
  mainPanel(
    
    tabsetPanel(id = 'tabs_1',
    
      tabPanel('Main',
               
              fluidRow(column(8, plotOutput("utilization_YM", 
                            click = clickOpts(id="util_YM_click"))),
                       column(2, plotOutput('Utilization_marginal1',
                            click = clickOpts(id = 'util_marginal1_click'))),
                       column(2, plotOutput('Utilization_marginal2'))
              ),
              
              fluidRow(
                
                
                rpivotTableOutput('pivot'),
                verbatimTextOutput('clicked'),
                dataTableOutput('utilization_users')
                
              )
      
               
      ),
      
      tabPanel('Compare', plotOutput('QA approval rate')
      
      ),
      
      tabPanel('User', plotOutput('utilization_user')
               
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