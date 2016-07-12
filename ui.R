

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
library(threejs)
library(RColorBrewer)
library(colorspace)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  titlePanel("Utilization"),
                  
                  sidebarPanel(width = 3,
                               
                               conditionalPanel(condition = "input.tabs_1 != 'Users - all' && input.tabs_1 != 'Selected'",
                                                
                                                
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
                                                
                                                uiOutput('reac_units')
                               ),
                               
                               conditionalPanel(condition = "input.tabs_1 == 'Group influence'",
                                                
                                                selectInput('influence_choice',
                                                            choices = c('Percent within category' = 'values',
                                                                        'Share from all users under statistic' = 'share',
                                                                        'Category influence' = 'whole',
                                                                        'Intervention impact' = 'intervention'),
                                                            multiple = F,
                                                            label = 'Measure',
                                                            selected = '1'),

                                                                 
                                                uiOutput('levelUI'),
                                               
                                                uiOutput('influence_hideUI'),
                              
                                                
                                                selectInput(inputId = 'influenceOpts',
                                                            label = 'Statistic',
                                                            choices = c('Mean' = 'mean',
                                                                        'Quantile' = 'quant'),
                                                            multiple = F,
                                                            selected = 'mean'
                                                ),
                                                
                                                conditionalPanel(condition = "input.influenceOpts == 'quant'",
                                                                 
                                                                 sliderInput(inputId = 'influenceQuantile',
                                                                             label = NULL,
                                                                             min = 0,
                                                                             max = 1,
                                                                             value = 0.5,
                                                                             step = 0.05
                                                                 )
                                                )
                               ),
                               
                               conditionalPanel(condition = "input.tabs_1 == 'Users - all'",
                                                uiOutput('user_list')
                               ),
                               
                               conditionalPanel(condition = "input.tabs_1 == 'Main'",
                                                
                                                
                                                
                                                selectInput(inputId = 'mainPlotVis',
                                                            label = 'Main plot',
                                                            choices = c('Boxplots', 
                                                                        'Point chart - user count',
                                                                        'Stacked barchart with counts', 
                                                                        'Stacked relative barchart', 
                                                                        'Dodged barchart'),
                                                            multiple = F,
                                                            selected = 'Boxplots'
                                                ),
                                                
                                                conditionalPanel(condition = "input.mainPlotVis == 'Point chart - user count'",
                                                                 
                                                                 selectInput(inputId = 'pointChartOpts',
                                                                             label = 'Y position',
                                                                             choices = c('Mean' = 'mean',
                                                                                         'Quantile' = 'quant'),
                                                                             multiple = F,
                                                                             selected = 'mean'
                                                                 ),
                                                                 
                                                                 conditionalPanel(condition = "input.pointChartOpts == 'quant'",
                                                                                  
                                                                                  sliderInput(inputId = 'pointChartQuantile',
                                                                                              label = NULL,
                                                                                              min = 0,
                                                                                              max = 1,
                                                                                              value = 0.5,
                                                                                              step = 0.05,
                                                                                              animate = animationOptions(interval = 500, loop = F)
                                                                                  )
                                                                 )
                                                ),
                                                
                                                conditionalPanel(condition = "input.mainPlotVis == 'Boxplots' || input.mainPlotVis == 'Point chart - user count'",
                                                                 checkboxInput(inputId = 'monthlySummaries',
                                                                               label = 'Monthly mean and median',
                                                                               value = F),
                                                                 checkboxInput('globalSummaries',
                                                                               label = 'Global monthly mean and median',
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
                                                
                                                selectInput(inputId = 'input_chartType',
                                                            choices = c('Faceted 2d scatter plot' = '2d',
                                                                        'Faceted 2d density plot' = 'densityFacet',
                                                                        'Faceted boxplot of binned x variable' = 'box',
                                                                        '2d density plot with all selected data' = 'densityAll',
                                                                        '3d scatter plot with all selected data' = '3d'),
                                                            selected = '2d',
                                                            label = 'Chart type',
                                                            multiple = F)
                               ),
                               
                               conditionalPanel(condition = "input.tabs_1 == 'Inputs' && (input.input_chartType == 'densityAll' || input.input_chartType == 'densityFacet')",
                                                
                                                selectInput('density_plotType',
                                                            choices = c('Polygon' = 'poly',
                                                                        'Heatmap' = 'heatmap'),
                                                            selected = 'poly',
                                                            label = NULL,
                                                            multiple = F)
                               ),
                               
                               conditionalPanel(condition = "input.tabs_1 == 'Inputs' && (input.input_chartType == '2d' || input.input_chartType == 'box' ||
                                                input.input_chartType == 'densityFacet')",
                                                
                                                checkboxInput('include_summary',
                                                              label = 'Include summary rows and columns',
                                                              value = F),
                                                
                                                selectInput(inputId = 'util_inputs',
                                                            choices = c('Tracked billable' = 't_bill',
                                                                        'Expected billable' = 'exp_bill',
                                                                        'Tracked investment' = 't_inv'),
                                                            label = 'X variable',
                                                            multiple = F,
                                                            selected = 't_bill')
                                                
                                                
                  ),
                  
                  conditionalPanel(condition = "input.tabs_1 == 'Inputs' && input.input_chartType == 'densityAll'",
                                   
                                   selectInput(inputId = 'util_inputs_dens',
                                               choices = c('Tracked billable' = 't_bill',
                                                           'Expected billable' = 'exp_bill',
                                                           'Tracked investment' = 't_inv'),
                                               label = 'X variable',
                                               multiple = F,
                                               selected = 't_bill')
                  ),
                  
                  conditionalPanel(condition = "input.tabs_1 == 'Inputs' && input.input_chartType == 'box'",
                                   
                                   selectInput(inputId = 'bin_option',
                                               choices = c('Split on values' = 'value',
                                                           'Split on quantiles' = 'quantile'),
                                               selected = 'quantile',
                                               label = NULL,
                                               multiple = F),
                                   
                                   sliderInput('box_bins',
                                               label = 'Nr. of bins',
                                               min = 2,
                                               max = 10,
                                               value = 5,
                                               step = 1),
                                   
                                   checkboxInput('boxlines', label = 'Include median and mean as lines', value = F)
                  ),
                  
                  
                  conditionalPanel(condition = "input.tabs_1 == 'Selected'",
                                   
                                   uiOutput('util_input_selected')
                                   
                  ),
                  
                  conditionalPanel(condition = "input.tabs_1 == 'Inputs' && input.input_chartType == '2d'",
                                   
                                   uiOutput('util_input_color')
                  ),
                  
                  conditionalPanel(condition = "input.tabs_1 == 'Selected'",
                                   
                                   uiOutput('util_selected_color'),
                                   
                                   sliderInput(inputId = 'psize',
                                               label = 'Point size',
                                               min = 1,
                                               max = 5,
                                               value = 3,
                                               step = 0.5,
                                               ticks = T)
                  ),
                  
                  conditionalPanel(condition = "(input.tabs_1 == 'Inputs'  && input.input_chartType == '2d') || input.tabs_1 == 'Selected'",
                                   
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
                  
                  ##################################################################################################
                  
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
                                                          column(2, plotOutput('Utilization_marginal2',
                                                                               click = clickOpts('marginal2_click')))
                                                 ),
                                                 
                                                 fluidRow(
                                                   #verbatimTextOutput('test'),
                                                   DT::dataTableOutput('all_units_statsYM')
                                                   
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        tabPanel('Group influence',
                                                 
                                                 tabsetPanel(id = 'GI',
                                                             
                                                           tabPanel('By month',
                                               
                                                               fluidRow(plotOutput('influence_plotMain')),
                                                               fluidRow(plotOutput('influence_plotDiff')),
                                                               fluidRow(plotOutput('influence_plotShare')),

                                                               verbatimTextOutput('test_influence')
                                               
                                                           ),
                                                 
                                                            tabPanel('Overall',
                                                                     
                                                                     fluidRow(plotOutput('influence_plotOverall')),
                                                                     fluidRow(plotOutput('influence_plotDiffOverall')),
                                                                     fluidRow(plotOutput('influence_plotShareOverall'))
                                                            ),
                                                             
                                                            tabPanel('Table',
                                                                     
                                                                     fluidRow(DT::dataTableOutput('influence_table'))
                                                                     
                                                            )
                                                             
                                                 )
                                        
                                        ),
                                        
                                        tabPanel('Inputs', 
                                                 
                                                 uiOutput('inputs_plot'),
                                                 
                                                 verbatimTextOutput('test_inputs'),
                                                 
                                                 verbatimTextOutput('test_inputs2'),
                                                 
                                                 conditionalPanel(condition = "input.tabs_1 == 'Inputs' && input.input_chartType == '3d'",
                                                                  
                                                                  absolutePanel(top = 50,
                                                                                right = 20,
                                                                                
                                                                                selectInput(inputId = 'three_x_var',
                                                                                            label = 'X variable',
                                                                                            choices = c('Tracked billable' = 't_bill',
                                                                                                        'Expected billable' = 'exp_bill',
                                                                                                        'Tracked investment' = 't_inv'),
                                                                                            selected = 't_bill',
                                                                                            selectize = T,
                                                                                            multiple = F,
                                                                                            width = '200px'),
                                                                                
                                                                                uiOutput('three_Y'),
                                                                                
                                                                                uiOutput('three_color'),
                                                                                
                                                                                uiOutput('three_size'),
                                                                                
                                                                                sliderInput(inputId = 'three_point_size',
                                                                                            label = 'Size of points',
                                                                                            min = 0.1,
                                                                                            max = 5,
                                                                                            step = 0.1,
                                                                                            value = 1,
                                                                                            width = '200px'),
                                                                                
                                                                                actionButton(inputId = 'render_three_button',
                                                                                             label = 'Render')
                                                                  )
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        tabPanel('Selected', 
                                                 
                                                 fluidRow(
                                                   column(9,
                                                          plotOutput('selected_chart', height = '600px',
                                                                     brush = brushOpts(id = 'selected_brush',
                                                                                       direction = 'xy',
                                                                                       clip = T, resetOnNew = T),
                                                                     dblclick = 'selected_dblclick',
                                                                     hover = hoverOpts('selected_hover',
                                                                                       delay = 700,
                                                                                       delayType = 'debounce',
                                                                                       nullOutside = F))
                                                   ),
                                                   
                                                   column(3,
                                                          tableOutput('hovUserYM')
                                                   )
                                                 ),
                                                 
                                                 tableOutput('test_hov'),
                                                 DT::dataTableOutput('selected_table'),
                                                 verbatimTextOutput('test1')
                                                 
                                        ),
                                        
                                        
                                        tabPanel('Users - all',
                                                 
                                                 dygraphOutput('users_dyg'),
                                                 HTML('threejs')
                                                 
                                        ),
                                        
                                        tabPanel('User countries',
                                                 
                                                 leafletOutput('countries',
                                                               height = '600px'),
                                                 
                                                 verbatimTextOutput('test2'),
                                                 verbatimTextOutput('test3'),
                                                 
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
                                                                                       'Quantile' = 'quantile'),
                                                                           multiple = F,
                                                                           selected = 'mean',
                                                                           selectize = T,
                                                                           width = '200px'),
                                                               
                                                               conditionalPanel(condition = "input.map_statistic == 'quantile'",
                                                                                
                                                                                sliderInput('map_quant',
                                                                                            label = NULL,
                                                                                            min = 0,
                                                                                            max = 1,
                                                                                            value = 0.5,
                                                                                            step = 0.05,
                                                                                            width = '200px'
                                                                                )
                                                               ),
                                                               
                                                               
                                                               uiOutput('circleVar'),
                                                               
                                                               conditionalPanel(condition = "input.circle_variable != 'YM_meanCount'",
                                                                                
                                                                                selectInput('circle_statistic',
                                                                                            label = NULL,
                                                                                            choices = c('Mean' = 'mean',
                                                                                                        'Quantile' = 'quantile'),
                                                                                            multiple = F,
                                                                                            selected = 'mean',
                                                                                            selectize = T,
                                                                                            width = '200px'),
                                                                                
                                                                                
                                                                                
                                                                                conditionalPanel(condition = "input.circle_statistic == 'quantile'",
                                                                                                 
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
                                                               
                                                               
                                                 )
                                        ),
                                        
                                        tabPanel('Pivot', rpivotTableOutput('main_table')
                                                 
                                                 
                                                 
                                        )
                                        
                            )
                  )
))
