
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dygraphs)
library(dplyr)
library(lubridate)
library(rpivotTable)
library(shinythemes)
library(leaflet)

shinyUI(fluidPage(theme = shinytheme("Spacelab"),
  
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
                                choices = c('Boxplots', 
                                            'Point chart - mean, user count',
                                            'Stacked barchart with counts', 
                                            'Stacked relative barchart', 
                                            'Dodged barchart'),
                                multiple = F,
                                selected = 'Boxplots'
                    ),
                    
                    conditionalPanel(condition = "input.mainPlotVis == 'Boxplots' || input.mainPlotVis == 'Point chart - mean, user count'",
                                     checkboxInput(inputId = 'monthlySummaries',
                                                   label = 'Include monthly mean and median',
                                                    value = F)
                    ),
                    
                    selectInput(inputId = 'marginalVis',
                                  label = 'Marginal plot',
                                  choices = c('Boxplots', 'Stacked histograms', 'Stacked relative barchart', 'Overlaid histograms'),
                                  multiple = F,
                                  selected = 'Boxplots'
                    ),
                    
                    sliderInput(inputId = 'bins',
                                label = 'Nr. of bins',
                                min = 10,
                                max = 100,
                                value = 30,
                                step = 1
                    )            
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Inputs'",
                     
                     selectInput(inputId = 'util_inputs',
                                 choices = c('Tracked billable' = 't_bill',
                                             'Expected billable' = 'exp_bill',
                                             'Tracked investment' = 't_inv'),
                                 label = 'X variable',
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
                     
                     uiOutput('util_selected_color'),
                     
                     sliderInput(inputId = 'psize',
                                 label = 'Point size',
                                 min = 1,
                                 max = 5,
                                 value = 2,
                                 step = 0.5,
                                 ticks = T)
    ),
    
    conditionalPanel(condition = "input.tabs_1 == 'Inputs' || input.tabs_1 == 'Selected'",
                     
                     sliderInput(inputId = 'alpha',
                                 label = 'Alpha',
                                 min = 1,
                                 max = 100,
                                 value = 5,
                                 step = 1),
                     
                     selectInput(inputId = 'smooth',
                                   label = 'Smoothing',
                                   choices = c('Conditional mean' = 'mean',
                                               'Linear regression' = 'regression',
                                               'None' = 'none'),
                                 multiple = F,
                                 selected = 'none')
    )

  ),
  
  mainPanel(width = 9,
    
    tabsetPanel(id = 'tabs_1',
    
      tabPanel('Main',
               
              fluidRow(column(8, plotOutput("utilization_YM",
                                            click = clickOpts('main_click'))),
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
               
               fluidRow(
                 column(9,
                   plotOutput('selected_chart', height = '600px',
                              brush = brushOpts(id = 'selected_brush',
                                                direction = 'xy',
                                                clip = T),
                              dblclick = 'selected_dblclick',
                              hover = hoverOpts('selected_hover',
                                                delay = 500,
                                                delayType = 'debounce',
                                                nullOutside = F))
                   ),
                 
                 column(3,
                    tableOutput('hovUserYM')
                  )
               ),
               
               tableOutput('test_hov'),
               DT:: dataTableOutput('selected_table'),
               verbatimTextOutput('test1')
               
      ),
      
      tabPanel('Group influence',
               
               plotOutput('utilization_compare')
        
      ),
      
      tabPanel('Users - all',
               
               dygraphOutput('users_dyg')
      ),
      
      tabPanel('Map',
               
               leafletOutput('countries',
                             height = '600px'),
               
               verbatimTextOutput('test2'),
               
               DT::dataTableOutput('usersCountry_table'),
               
               absolutePanel(top = 50,
                             right = 20,
                             
                             selectInput('map_variable',
                                         label = 'Color variable',
                                         choices = c('Utilization' = 'util_bill',
                                                     'Tracked billable' = 't_bill',
                                                     'Tracked investment' = 't_inv',
                                                     'Expected billable' = 'exp_bill'),
                                         multiple = F,
                                         selectize = T,
                                         selected = 'util_bill',
                                         width = '200px'),
                             
                             selectInput('map_statistic',
                                         label = NULL,
                                         choices = c('Mean' = 'mean',
                                                     'Percentile' = 'percentile'),
                                         multiple = F,
                                         selected = 'mean',
                                         selectize = T,
                                         width = '200px'),
                             
                             conditionalPanel(condition = "input.map_statistic == 'percentile'",
                                              
                                sliderInput('map_quant',
                                            label = NULL,
                                            min = 0,
                                            max = 1,
                                            value = 0.5,
                                            step = 0.05,
                                            width = '200px'
                                )
                             ),
                             
                             checkboxInput(inputId = 'includeCircle',
                                           label = 'Include circles',
                                           value = F,
                                           width = '200px'
                              ),
                             
                             conditionalPanel(condition = "input.includeCircle == true",
                             
                                uiOutput('circleVar',
                                        width = '200px'),
                                
                                selectInput('circle_statistic',
                                            label = NULL,
                                            choices = c('Mean' = 'mean',
                                                        'Percentile' = 'percentile'),
                                            multiple = F,
                                            selected = 'mean',
                                            selectize = T,
                                            width = '200px')
                                
                                
                             ),
                             
                             conditionalPanel(condition = "input.includeCircle == true & input.circle_statistic == 'percentile'",

                                              sliderInput('circle_quant',
                                                          label = NULL,
                                                          min = 0,
                                                          max = 1,
                                                          value = 0.5,
                                                          step = 0.05,
                                                          width = '200px'
                                              )

                             )
                             
                             
               )
      ),
      
      tabPanel('Pivot', rpivotTableOutput('main_table')
               
               
      
      )
      
    )
  )
))

