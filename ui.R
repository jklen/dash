
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rpivotTable)

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
    
    selectInput(inputId = 'mainPlotVis',
                label = 'Main plot',
                choices = c('Boxplots', 'Stacked barchart with counts', 'Stacked relative barchart'),
                multiple = F,
                selected = 'Boxplots'
    ),
    
    selectInput(inputId = 'marginalVis',
                  label = 'Marginal plot',
                  choices = c('Boxplots', 'Stacked histograms', 'Stacked relative barchart', 'Overlaid histograms'),
                  multiple = F,
                  selected = 'Boxplots'
    )
    
  ),
  
  mainPanel(
    
    tabsetPanel(id = 'tabs_1',
    
      tabPanel('Main',
               
              fluidRow(column(8, plotOutput("utilization_YM", 
                            click = 'mainPlot_click' )),
                       column(2, plotOutput('Utilization_marginal1',
                            brush = brushOpts(id = 'marginal1_brush', direction = 'y'),
                            dblclick = 'marginal1_dblclick')),
                       column(2, plotOutput('Utilization_marginal2'))
              ),
              
              fluidRow(
                verbatimTextOutput('test'),
                column(8, verbatimTextOutput('main_summary')),
                column(4, verbatimTextOutput('marginal_summary'), verbatimTextOutput('marginal_summary2')),
                dataTableOutput('utilization_users')
                
              )
      
               
      ),
      
      tabPanel('Pivot', rpivotTableOutput('main_table')
      
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