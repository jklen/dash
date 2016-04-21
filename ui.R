
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dygraphs)
library(dplyr)
library(lubridate)
library(rpivotTable)

shinyUI(fluidPage(
  
  titlePanel("Utilization"),
  
  sidebarPanel(width = 3,
               
    conditionalPanel(condition = "input.tabs_1 != 'Users - all'",
                                
    
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
                            label = "User utilization for", 
                            choices = c('Geo' = 'GEO_NAME',
                                        'Organization' = 'ORG_NAME',
                                        'Department' = 'DEPT_NAME')
                  ),
                  
                  div(style = 'height: 220px; overflow:scroll', uiOutput('reac_units'))
                ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Users - all'",
                     uiOutput('user_list')
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Main'",
                     
                     
    
                    selectInput(inputId = 'mainPlotVis',
                                label = 'Main plot',
                                choices = c('Boxplots', 'Stacked barchart with counts', 'Stacked relative barchart', 'Dodged barchart'),
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
    
    conditionalPanel(condition = "input.tabs_1 == 'Inputs'",
                     
                     selectInput(inputId = 'util_inputs',
                                 choices = c('Tracked billable' = 't_bill',
                                             'Expected billable' = 'exp_bill',
                                             'Tracked investment' = 't_inv'),
                                 label = 'Y variable',
                                 multiple = F,
                                 selected = 't_bill')
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Selected'",
                     
                     uiOutput('util_input_selected')
                     
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Inputs'",
                     
                     uiOutput('util_input_color')
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Selected'",
                     
                     uiOutput('util_selected_color')
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Inputs' || input.tabs_1 == 'Selected'",
                     
                     sliderInput(inputId = 'alpha',
                                 label = 'Alpha',
                                 min = 1,
                                 max = 100,
                                 value = 5,
                                 step = 1),
                     
                     checkboxInput(inputId = 'smooth',
                                   label = 'Add regression line',
                                   value = F)
    )

  ),
  
  mainPanel(width = 9,
    
    tabsetPanel(id = 'tabs_1',
    
      tabPanel('Main',
               
              fluidRow(column(8, plotOutput("utilization_YM")),
                       column(2, plotOutput('Utilization_marginal1',
                            brush = brushOpts(id = 'marginal1_brush', 
                                              direction = 'y',
                                              resetOnNew = T),
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
      
      tabPanel('Inputs', 
               
               plotOutput('utilization_inputs',
                              height = "1200px",
                              brush = brushOpts(id = 'inputs_brush',
                                                direction = 'xy',
                                                clip = T)

                        )
               

      ),
      
      tabPanel('Selected', 
               
               plotOutput('selected_chart'),
               DT:: dataTableOutput('selected_table'),
               verbatimTextOutput('test1')
               
      ),
      
      tabPanel('Group influence',
               
               plotOutput('utilization_compare')
        
      ),
      
      tabPanel('Users - all',
               
               dygraphOutput('users_dyg')
      ),
      
      tabPanel('Pivot', rpivotTableOutput('main_table')
      
      )
      
    )
  )
))

