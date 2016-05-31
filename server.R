
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(dygraphs)
library(xts)
library(tidyr)
library(dplyr)
library(lubridate)
library(rpivotTable)
library(leaflet)
library(rgdal)
library(rgeos)
library(lazyeval)
library(threejs)
library(RColorBrewer)
library(colorspace)

# data load

load('.Rdata')

summaryfunction <- function (x){
  if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    "Min." = round(as.numeric( min(x, na.rm = T)), 2),
    "1st Qu." = round(quantile(x, na.rm = T)[2], 2),
    "Median" = round(median(x, na.rm = T), 2),
    "Mean" = round(mean(x, na.rm = T), 2),
    "3rd Qu." = round(quantile(x, na.rm = T)[4], 2),
    "Max." = round(max(x, na.rm = T), 2),
    row.names=""
    
  )
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  return( mysummary )
}

shinyServer(function(input, output, session) {
  
  # rendering checkbox group based on reactive vector of units
  
  output$reac_units <- renderUI({
    
    units_list <- pass_units()
      
    selectInput(inputId = 'units',
                       label = input$grouping,
                       choices = units_list,
                       selectize = T,
                       multiple = T
                       )
      
  })
  
  # rendering listbox of possible color variables in Inputs tab
  
  output$util_input_color <- renderUI({
    
    color_var_list <- pass_color_var()
    
    selectInput(inputId = 'color_var',
                label = 'Color variable',
                choices = c(color_var_list, 'None' = 'none'),
                selected = 't_inv',
                selectize = T,
                multiple = F)
    
  })
  
  # rendering listbox of possible color variables in Selected tab
  
  output$util_selected_color <- renderUI({
    
    color_var_list_select <- pass_color_var_selected()
    
    selectInput(inputId = 'color_var_select',
                label = 'Color variable',
                choices = color_var_list_select,
                selected = input$grouping,
                selectize = T,
                multiple = F
    )
    
  })
  
  # rendering listbox of possible X variables in Selected tab
  
  output$util_input_selected <- renderUI({
    
    selectInput(inputId = 'util_inputs_selected',
                choices = c('Tracked billable' = 't_bill',
                            'Expected billable' = 'exp_bill',
                            'Tracked investment' = 't_inv'),
                label = 'X variable',
                multiple = F,
                selected = input$util_inputs
    )
    
  })
  
  output$user_list <- renderUI({
    
    selectInput(inputId = 'user_list_select',
                label = 'Select users',
                choices = unique(df_util$USER_NAME),
                selectize = T,
                multiple = T
    )
    
  })
  
  pass_color_var <- reactive({
    
    color_vars <- c('Tracked billable' = 't_bill',
                    'Expected billable' = 'exp_bill',
                    'Tracked investment' = 't_inv')
    
    color_vars <- color_vars[color_vars != input$util_inputs]
    
    color_vars
    
  })
  
  pass_color_var_selected <- reactive({
    
    color_vars_selected <- c('Tracked billable' = 't_bill',
                             'Expected billable' = 'exp_bill',
                             'Tracked investment' = 't_inv',
                             switch(input$grouping, GEO_NAME = c('Geo' = 'GEO_NAME'),
                                    ORG_NAME = c('Organization' = 'ORG_NAME'),
                                    DEPT_NAME = c('Department' = 'DEPT_NAME')),
                             'User' = 'USER_NAME',
                             'None' = 'none')
    
    color_vars_selected <- color_vars_selected[color_vars_selected != input$util_inputs_selected]
    
    color_vars_selected
    
  })
  
 
  # creating reactive vector of units based on selected groupings
  
  pass_units <- reactive({
    
    if (input$grouping == 'GEO_NAME'){
      units <- unique(pass_df()$GEO_NAME)
    } else {
      
      if (input$grouping == 'ORG_NAME'){
        units <- unique(pass_df()$ORG_NAME) 
      } else {
        
        if (input$grouping == 'DEPT_NAME'){
          units <- unique(pass_df()$DEPT_NAME) 
        } else {
          units <- unique(pass_df()$USER_NAME) 
        }
      }
    }
    
    #cat(file=stderr(), "selected grouping - ", input$grouping) # vypise do konzoly

  units  
    
  })
  

  output$three_Y <- renderUI({
    
    vars <- c('Tracked billable' = 't_bill',
               'Expected billable' = 'exp_bill',
               'Tracked investment' = 't_inv')
    
    vars <- vars[vars != input$three_x_var]
    
    selectInput(inputId = 'three_y_var',
                label = 'Y variable',
                choices = vars,
                selected = 't_inv',
                multiple = F,
                width = '200px')
    
  })
  
  output$three_color <- renderUI({
    
    vars <- c('Tracked billable' = 't_bill',
              'Expected billable' = 'exp_bill',
              'Tracked investment' = 't_inv',
              switch(input$grouping, GEO_NAME = c('Geo' = 'GEO_NAME'),
                     ORG_NAME = c('Organization' = 'ORG_NAME'),
                     DEPT_NAME = c('Department' = 'DEPT_NAME')))
    
    vars <- vars[!(vars %in% c(input$three_x_var, input$three_y_var))]
    
    selectInput(inputId = 'three_color_var',
                label = 'Color variable',
                choices = c(vars, 'None' = 'none'),
                selected = 'none',
                multiple = F,
                width = '200px')
    
  })
  
  output$three_size <- renderUI({
    
    vars <- c('Tracked billable' = 't_bill',
              'Expected billable' = 'exp_bill',
              'Tracked investment' = 't_inv')
    
    vars <- vars[!(vars %in% c(input$three_x_var, input$three_y_var, input$three_color_var))]
    
    selectInput(inputId = 'three_size_var',
                label = 'Point size variable',
                choices = c(vars, 'None' = 'none'),
                selected = 'none',
                multiple = F,
                width = '200px')
    
  })
  
  # circle size in map
  
  output$circleVar <- renderUI({
    
    vars <- c('Utilization' = 'util_bill',
              'Tracked billable' = 't_bill',
              'Expected billable' = 'exp_bill',
              'Tracked investment' = 't_inv',
              'Users mean count' = 'YM_meanCount')
    
    vars <- vars[vars != input$map_variable]
    
    selectInput(inputId = 'circle_variable',
                label = 'Circle variable',
                choices = vars,
                selected = 'YM_meanCount',
                multiple = F,
                width = '200px')
    
  })
  
  # creating reactive dataframe based on date and value (units in prompt updated)
  
  pass_df <- reactive({
   
    df_util_r <- df_util[between(df_util$YEARMONTH, as.POSIXct(input$date_range[1]) - days(1), as.POSIXct(input$date_range[2])),]
    df_util_r <- df_util_r[between(df_util_r$util_bill, input$util_value[1], input$util_value[2]),]
    
    df_util_r$YEARMONTH <- as.factor(df_util_r$YEARMONTH)
    
    df_util_r
    
  })

  dfToPlot <- reactiveValues(df = NULL)
  

  
  observeEvent(input$units, {
    
     #cat(file=stderr(), "-------------dfUNITS---------", input$units)
     #cat(file=stderr(), "-------------dfGROUPING---------", input$grouping)
    
    
    df_util_reac <- pass_df()
    
      if (input$grouping == 'GEO_NAME'){
        
        df_util_reac <- df_util_reac[df_util_reac$GEO_NAME %in% input$units,]
        
        #cat(file=stderr(), "-------------DF---------", unique(dfToPlot$df$GEO_NAME))
        
      } else {
        
        if (input$grouping == 'ORG_NAME'){
          
          df_util_reac <- df_util_reac[df_util_reac$ORG_NAME %in% input$units,]
          
        } else {
          
          if (input$grouping == 'DEPT_NAME'){
            
            df_util_reac <- df_util_reac[df_util_reac$DEPT_NAME %in% input$units,]
            
          } else {
            
            if (input$grouping == 'USER_NAME'){
              
              df_util_reac <- df_util_reac[df_util_reac$USER_NAME %in% input$units,]
              
            }
            
          }
          
        }
        
      }
    
    # group reactive dataframe based on selected grouping
    
    
    groups_toPlot <- c(input$grouping, 'YEARMONTH', 'USER_NAME', 'COUNTRY_NAME') 
    
    if (!is.null(df_util_reac)){
      
      df_util_reac <- df_util_reac %>% 
        group_by_(.dots = lapply(groups_toPlot, as.symbol)) %>%
        summarise(t_bill = sum(t_bill), 
                  t_inv = sum(t_inv), 
                  exp_bill = sum(exp_bill)) %>%
        mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
        ungroup()
    }
    
    dfToPlot$df <- df_util_reac
    

  })
  
  brushed <- reactiveValues(df = NULL)
  
  observeEvent(input$inputs_brush, {
    
    req(input$units) # because change in grouping => nullify units => error
    
    df <- dfToPlot$df
    
    isolate(
    
    if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 != '(all)'){
      
     brushed$df <- df[df$YEARMONTH == input$inputs_brush$panelvar2 &
                             df$util_bill >= input$inputs_brush$ymin &
                             df$util_bill <= input$inputs_brush$ymax &
                             df[input$util_inputs] >= input$inputs_brush$xmin &
                             df[input$util_inputs] <= input$inputs_brush$xmax, ]
      
    } else {
      
      if (input$inputs_brush$panelvar1 != '(all)' & input$inputs_brush$panelvar2 == '(all)'){
        
        brushed$df <- df[df[input$grouping] == input$inputs_brush$panelvar1 &
                               df$util_bill >= input$inputs_brush$ymin &
                               df$util_bill <= input$inputs_brush$ymax &
                               df[input$util_inputs] >= input$inputs_brush$xmin &
                               df[input$util_inputs] <= input$inputs_brush$xmax, ] 
        
      } else {
        
        if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 == '(all)'){
          
          brushed$df <- df[df$util_bill >= input$inputs_brush$ymin &
                                 df$util_bill <= input$inputs_brush$ymax &
                                 df[input$util_inputs] >= input$inputs_brush$xmin &
                                 df[input$util_inputs] <= input$inputs_brush$xmax, ] 
          
        } else {
          
          brushed$df <- brushedPoints(df, 
                                          input$inputs_brush,
                                          input$util_inputs,
                                          'util_bill')
          
        }
      }
      
    }
    
    )
    #selected_brush
    
  })

  output$Utilization_marginal1 <- renderPlot({
    
    df <- dfToPlot$df
    
    if (!is.null(dfToPlot$df) & !is.null(input$units)){
    
      #cat(file=stderr(), "-------------PLOTDF---------", unique(df$GEO_NAME))
      
      plot_util_marg1 <- ggplot(aes(x = util_bill), data = df) +
        geom_histogram(fill = '#F79420', color = 'black', bins = input$bins, alpha = 0.5) +
        geom_vline(xintercept = mean(df$util_bill)) +
        geom_vline(xintercept = as.numeric(mean(df$util_bill, na.rm = T)),
                   color = 'red') +
        geom_vline(xintercept = as.numeric(median(df$util_bill, na.rm = T)),
                   color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$util_bill,
                                                    probs = 0.25, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$util_bill,
                                                    probs = 0.75, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$util_bill,
                                                    probs = 0.1, na.rm = T)),
                   linetype = 3) +
        geom_vline(xintercept = as.numeric(quantile(df$util_bill,
                                                    probs = 0.9, na.rm = T)),
                   linetype = 3) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              axis.title.y = element_blank())
      
      if (!is.null(range)){
        
        plot_util_marg1 <- plot_util_marg1 + coord_flip(xlim = range$y)
        
      }
      
      plot_util_marg1
      
    }
      
    
    
  })
  
  ##############
  
  
  # for plot zooming - Main
  
  range <- reactiveValues(y = NULL)
  
  observeEvent(input$marginal1_dblclick, {
    
    brush <- input$marginal1_brush
    
    if (!is.null(brush)){
      
      range$y <- c(brush$ymin, brush$ymax)
      
    } else {
      
      range$y <- NULL 
      
    }
    
  })
  
  # for main plot in Main tab - otherwise error when changing grouping, same as with leaflet (CDS_ <- GEO_NAME)
  
  gr <- reactiveValues(x = NULL)
  
  observeEvent(input$units, {
    
    if(!is.null(input$units)){
      
      gr$x <- input$grouping
      
    } else {
      
      gr$x <- NULL
      
    }
    
  })


  # plot to output
  
  output$utilization_YM <- renderPlot({
    
    req(gr$x, input$units)
    
    df <- dfToPlot$df
    g <- gr$x

    if (input$mainPlotVis == 'Boxplots'){

      plot_util_YM <- ggplot(aes_string(y = 'util_bill', fill = g), data = df) + 
        geom_boxplot(aes(x = factor(YEARMONTH)), alpha = 0.5) +
        geom_point(aes(x = factor(YEARMONTH)), 
                   position=position_dodge(width=0.75), 
                   fun.y = mean, stat = 'summary', shape = 1) +
        geom_point(aes(x = factor(YEARMONTH)), 
                   position=position_dodge(width=0.75), 
                   fun.y = quantile, fun.args=list(probs=0.1),
                  stat = 'summary', shape = 4) +
        geom_point(aes(x = factor(YEARMONTH)), 
                   position=position_dodge(width=0.75), 
                   fun.y = quantile, fun.args=list(probs=0.9),
                  stat = 'summary', shape = 4) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'))
      
      if (input$monthlySummaries == T){
        
        plot_util_YM <- plot_util_YM +
          geom_line(aes(x = factor(YEARMONTH), group = g), 
                    fun.y = mean, stat = 'summary', color = 'red', alpha = 0.4, size =2) +
          geom_line(aes(x = factor(YEARMONTH), group = g), 
                    fun.y = median, stat = 'summary', color = 'blue', alpha = 0.4, size =2)
          
      }
      
  
    } else {
     
      if (input$mainPlotVis == 'Stacked barchart with counts') {
        
        plot_util_YM <- ggplot(aes_string(fill = g), data = df) +
          geom_bar(aes(x = factor(YEARMONTH)), position = 'stack', alpha = 0.5) +
          theme(panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'))
        
      } else {
        
        if (input$mainPlotVis == 'Stacked relative barchart'){
          
          plot_util_YM <- ggplot(aes_string(fill = g), data = df) +
            geom_bar(aes(x = factor(YEARMONTH)), position = 'fill', alpha = 0.5) +
            theme(panel.background = element_rect(fill =NA),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  axis.line = element_line(colour = '#BDBDBD'))
          
        } else {
          
          if (input$mainPlotVis == 'Dodged barchart'){
            
            plot_util_YM <- ggplot(aes_string(fill = g), data = df) +
              geom_bar(aes(x = factor(YEARMONTH)),position = 'dodge', alpha = 0.5) +
              theme(panel.background = element_rect(fill =NA),
                    panel.grid.major = element_line(colour = '#e5e5e5'),
                    axis.line = element_line(colour = '#BDBDBD')) 
            
          } else {
            
            if (input$mainPlotVis == 'Point chart - mean, user count'){
            
              plot_util_YM <- ggplot(data = NULL) +
                geom_point(aes_string(x = 'YEARMONTH', y = 'm_util_bill', size = 'count', color = g),
                           alpha = 0.4,
                           position = position_dodge(width = 0.75),
                           data = df %>% 
                             group_by_(.dots = lapply(c('YEARMONTH', g), as.symbol)) %>%
                             summarise(count = n(), m_util_bill = mean(util_bill)) %>%
                             ungroup()) +
                scale_size_continuous(range = c(1,20)) +
                  theme(panel.background = element_rect(fill =NA),
                        panel.grid.major = element_line(colour = '#e5e5e5'),
                        axis.line = element_line(colour = '#BDBDBD'))
              
              if (input$monthlySummaries == T){
                
                plot_util_YM <- plot_util_YM +
                    geom_line(aes(x = YEARMONTH, y = util_bill, group = g),
                              data = df,
                              fun.y = mean,
                              stat = 'summary',
                              color = 'red',
                              alpha = 0.4,
                              size = 2) +
                    geom_line(aes(x = YEARMONTH, y = util_bill, group = g),
                              data = df,
                              fun.y = median,
                              stat = 'summary',
                              color = 'blue',
                              alpha = 0.4,
                              size = 2)
                
              }
              
              
            }
            
          }
          
        }
        
      }
      
    }
          
    
    
    
        
    if (!is.null(range) & input$mainPlotVis == 'Boxplots'){
      
      plot_util_YM <- plot_util_YM + coord_cartesian(ylim = range$y)
      
    }
    
    
    
    plot_util_YM
    
  })
  
 
  
  output$Utilization_marginal2 <- renderPlot({
    
    req(input$grouping, input$units, dfToPlot$df)
    
    df <- dfToPlot$df
    g <- input$grouping
    
    if (!is.null(input$units)){
      
      if (input$marginalVis == 'Boxplots'){
        
        plot_util_marg2 <- ggplot(aes_string(y = 'util_bill', x = g, fill = g), data = df) +
          geom_boxplot(alpha = 0.5) +
          geom_point(fun.y = mean, stat = 'summary', shape = 1) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.1),
                     stat = 'summary', shape = 4) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.9),
                     stat = 'summary', shape = 4) +
          geom_hline(yintercept = as.numeric(mean(df$util_bill, na.rm = T)),
                     color = 'red') +
          geom_hline(yintercept = as.numeric(median(df$util_bill, na.rm = T)),
                     color = 'blue') +
          theme(legend.position = 'none',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'),
                axis.title.y = element_blank())
        
        if (!is.null(range)){
          
          plot_util_marg2 <- plot_util_marg2 + coord_cartesian(ylim = range$y)
          
        }
        
        plot_util_marg2
        
      
      } else {
        
        if (input$marginalVis == 'Overlaid histograms'){

            plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = g), data = df) +
            geom_histogram(alpha = 0.4, position = 'identity', bins = input$bins) + 
            geom_vline(xintercept = as.numeric(mean(df$util_bill, na.rm = T)),
                       color = 'red') +
            geom_vline(xintercept = as.numeric(median(df$util_bill, na.rm = T)),
                       color = 'blue') +
            theme(legend.position = 'none',
                  panel.background = element_rect(fill =NA),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  axis.line = element_line(colour = '#BDBDBD'),
                  axis.title.y = element_blank())
            
            if (!is.null(range)){
              
              plot_util_marg2 <- plot_util_marg2 + coord_flip(xlim = range$y)
              
            }
            
            plot_util_marg2
            
        } else {
            
          if (input$marginalVis == 'Stacked histograms'){
            
            plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = g), data = df) +
              geom_histogram(bins = input$bins, alpha = 0.5) +
              geom_vline(xintercept = as.numeric(mean(df$util_bill, na.rm = T)),
                         color = 'red') +
              geom_vline(xintercept = as.numeric(median(df$util_bill, na.rm = T)),
                         color = 'blue') +
              theme(legend.position = 'none',
                    panel.background = element_rect(fill =NA),
                    panel.grid.major = element_line(colour = '#e5e5e5'),
                    axis.line = element_line(colour = '#BDBDBD'),
                    axis.title.y = element_blank())
            
            if (!is.null(range)){
              
              plot_util_marg2 <- plot_util_marg2 + coord_flip(xlim = range$y)
              
            }
            
            plot_util_marg2
              
            
            
          } else {
            
            if (input$marginalVis == 'Stacked relative barchart'){
              
              plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = g), data = df) +
                geom_histogram(position = 'fill', bins = input$bins, alpha = 0.5) +
                ylab('%') +
                geom_vline(xintercept = as.numeric(mean(df$util_bill, na.rm = T)),
                           color = 'red') +
                geom_vline(xintercept = as.numeric(median(df$util_bill, na.rm = T)),
                           color = 'blue') +
                theme(legend.position = 'none',
                      panel.background = element_rect(fill =NA),
                      panel.grid.major = element_line(colour = '#e5e5e5'),
                      axis.line = element_line(colour = '#BDBDBD'),
                      axis.title.y = element_blank())
              
              if (!is.null(range)){
                
                plot_util_marg2 <- plot_util_marg2 + coord_flip(xlim = range$y)
                
              }
              
              plot_util_marg2
              
            }
            
          }
          
        }
        
      }
      
    }
    
  })
  
  
  # inputs tab plot
  
  output$inputs_plot <- renderUI({
    
    if (input$input_chartType == '2d'){
      
      plotOutput('utilization_inputs',
                 height = "1200px",
                 brush = brushOpts(id = 'inputs_brush',
                                   direction = 'xy',
                                   clip = T)
                 
      )
      
    } else {
      
      if (input$input_chartType == '3d'){
      
        scatterplotThreeOutput('three',
                             height = '700px')
        
      } else {
        
        plotlyOutput('utilization_inputs_box',
                   height = '1200px')
        
      }
      
    }
    
  })
  
  
  output$utilization_inputs <- renderPlot({
    
    req(input$units, input$color_var, input$util_inputs, input$alpha)
    
    df <- dfToPlot$df
    
    # isolate causes here, that error 'Error in layout_base: At least one layer must contain all variables used for facetting'
    #   when grouping is changed and unis nullified, does not occur
    
    isolate(

      if (input$color_var == 'none'){
        
        plot_util_rel <- ggplot(aes_string(x = input$util_inputs, y = 'util_bill'), data = df) +
          geom_point(alpha = 1/input$alpha, position = 'jitter') +
          theme(#aspect.ratio  = 1, - bug pri brushingu
            legend.position = 'none',
            panel.background = element_rect(fill =NA),
            panel.grid.major = element_line(colour = '#e5e5e5'),
            axis.line = element_line(colour = '#BDBDBD'),
            strip.background = element_rect(fill = '#e5e5ff'),
            strip.text = element_text(face = 'bold'))
        
      } else {
        
        if (input$color_var != 'none'){
          
          plot_util_rel <- ggplot(aes_string(x = input$util_inputs, y = 'util_bill', color = input$color_var), data = df) +
            geom_point(alpha = 1/input$alpha, position = 'jitter') +
            scale_colour_gradientn(colours=rainbow(5)) +
            theme(#aspect.ratio  = 1, - bug pri brushingu
              #legend.position = 'none',
              panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold')) 
          
        }
      }
      
    )
    
    if (input$include_summary == T){
      
      plot_util_rel <- plot_util_rel +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)), margins = T)
      
    } else {
      
      plot_util_rel <- plot_util_rel +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)))
      
    }
      
    if (input$smooth == 'regression'){
      
      plot_util_rel <- plot_util_rel + geom_smooth(method = 'lm', alpha = 0.05) 
      
    } else {
      
      if (input$smooth == 'mean'){
      
        plot_util_rel <- plot_util_rel + geom_smooth(alpha = 0.05)
        
      }
      
    }
      
    
      
    plot_util_rel
      
    
  })
  
  output$test_inputs <- renderPrint({
    
    event.data <- event_data("plotly_click", source = "plotly1")
    event.data
    
  })
  
  output$test_inputs2 <- renderPrint({
    
    event.data <- event_data("plotly_selected", source = "plotly1")
    event.data
    
  })
  
  output$utilization_inputs_box <- renderPlotly({
    
    req(input$units, input$util_inputs)
    
    df <- dfToPlot$df
    
    
    
    # binning

    if (input$bin_option == 'quantile'){

      var_cuts <- quantile(df[, input$util_inputs][[input$util_inputs]], (0:input$box_bins)/input$box_bins, na.rm = T)
      var_binned <- cut(df[, input$util_inputs][[input$util_inputs]], var_cuts, include.lowest =  T)
      df$xBinned <- var_binned
      #
    } else {

      var_binned <- cut(df[, input$util_inputs][[input$util_inputs]],
                        input$box_bins, include.lowest = T)
      df$xBinned <- var_binned

    }
    
    bin_plot <- ggplot(aes_string(x = 'xBinned', y = 'util_bill', fill = input$grouping), data = df) +
      geom_boxplot(alpha = 0.4) +
      geom_point(fun.y = mean, stat = 'summary', shape = 1) +
      coord_cartesian(ylim = c(0,1.5)) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill =NA),
            panel.grid.major = element_line(colour = '#e5e5e5'),
            axis.line = element_line(colour = '#BDBDBD'),
            strip.background = element_rect(fill = '#e5e5ff'),
            strip.text = element_text(face = 'bold')) +
      xlab(input$util_inputs)
    
    if (input$include_summary == T){
      
      bin_plot <- bin_plot +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)), margins = T)
      
    } else {
      
      bin_plot <- bin_plot +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)))
      
    }
    
    if (input$boxlines == T){
      
      bin_plot <- bin_plot +
      geom_line(fun.y = mean, stat = 'summary', size = 1, alpha = 0.4, aes(group = input$grouping), color = 'red') +
        geom_line(fun.y = median, stat = 'summary', size = 1, alpha = 0.4, aes(group = input$grouping), color = 'blue')
      
    }
    
    bin_plot <- ggplotly(bin_plot, source = 'plotly1')
    bin_plot
    
  })
  
  output$three <- renderScatterplotThree({
    
    req(input$units) # causes when changing grouping and units are nullified, the 3d chart dissapears
    
    input$render_three_button
    
    df <- dfToPlot$df
    
    isolate(
    
      if (input$grouping == input$three_color_var){
        
        three_color = brewer.pal(n = length(unique(df[[input$three_color_var]])), 'Set1')[factor(df[[input$three_color_var]])]
        #three_color = rainbow_hcl(length(unique(df[[input$three_color_var]])))[factor(df[[input$three_color_var]])]
        
        
      } else {
        
        if (input$three_color_var != 'none'){
          
          three_color <- colorRampPalette(brewer.pal(name = 'RdYlGn', n = 11))(max(df[[input$three_color_var]]))[df[[input$three_color_var]]]
          
        } else {
          
          three_color <- NULL
          
        }
        
      }
      
     
    )
    
    isolate(
      
      if (input$three_size_var == 'none'){
        
        three_size <- input$three_point_size
        
      } else {
        
        # cut variable for point size to 10 intervals
        
        v_cutted <- cut(df[[input$three_size_var]], 
                        seq(from = min(df[[input$three_size_var]]), 
                            to = max(df[[input$three_size_var]]), 
                            length.out = 11))
        
        # create vector with point size based on v_cutted
        
        three_size <- (seq(from = 0.2, by = 0.1, length.out = 10)[as.numeric(v_cutted)]) * input$three_point_size
        
      }
      
    )
    
    chartToPlot <- isolate(scatterplot3js(x = df[[input$three_x_var]], 
                                  y = df[[input$three_y_var]], 
                                  z = df$util_bill, 
                                  color = three_color, 
                                  size = three_size,
                                  renderer = 'canvas'))
    if (is.null(dfToPlot$df)){
      
      chartToPlot <- NULL
      
    }
    
    chartToPlot
    
  })
  
  # chart in Selected tab
  
  output$selected_chart <- renderPlot ({
    
    req(input$units, brushed$df, gr$x, input$util_inputs_selected, input$color_var_select)
    
    df <- brushed$df
    g <- gr$x


    if (!is.null(input$inputs_brush)){
      
      if (input$color_var_select %in% c('none', 'USER_NAME')){
        
        plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'util_bill'),
                                data = df) +
          geom_point(alpha = 1/input$alpha, position = 'jitter', size = input$psize) +
          theme(legend.position = 'bottom',
            panel.background = element_rect(fill =NA),
            panel.grid.major = element_line(colour = '#e5e5e5'),
            axis.line = element_line(colour = '#BDBDBD'),
            strip.background = element_rect(fill = '#e5e5ff'),
            strip.text = element_text(face = 'bold'))
        
        #plot_selected <- plot_selected + geom_point(alpha = 1/input$alpha, position = 'jitter')
        
      } else {
        
        if (input$color_var_select != g){
          
          plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'util_bill', color = input$color_var_select),
                                  data = df) +
            geom_point(alpha = 1/input$alpha, position = 'jitter', size = input$psize) +
            scale_colour_gradientn(colours = rainbow(5)) +
            theme(legend.position = 'bottom',
              panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold'))
          
          
          # plot_selected <- plot_selected + 
          #   geom_point(aes_string(color = input$color_var_select), alpha = 1/input$alpha, position = 'jitter') +
          #   scale_colour_gradientn(colours = rainbow(5))
          
        } else {
          
          if (input$color_var_select == g){
            
            plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'util_bill', color = input$color_var_select),
                                    data = df) +
              geom_point(alpha = 1/input$alpha, position = 'jitter', size = input$psize) +
              theme(legend.position = 'bottom',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'),
                strip.background = element_rect(fill = '#e5e5ff'),
                strip.text = element_text(face = 'bold'))
            
            # plot_selected <- plot_selected + geom_point(aes_string(color = input$color_var_select), alpha = 1/input$alpha, position = 'jitter')
            
          }
          
        }
      }
      
      if (input$smooth == 'regression') {
        
        if (input$color_var_select == g){
        
          plot_selected <- plot_selected + geom_smooth(method = 'lm', aes_string(group = g), alpha = 0.05)
          
        } else {
          
          if (input$color_var_select != 'USER_NAME'){
            
            plot_selected <- plot_selected + geom_smooth(method = 'lm', alpha = 0.05) 
            
          }
          
        }
          
      } else {
        
        if (input$smooth == 'mean'){
          
          if (input$color_var_select == g){
            
            plot_selected <- plot_selected + geom_smooth(aes_string(group = g), alpha = 0.05)
            
          } else {
            
            if (input$color_var_select != 'USER_NAME'){
              
              plot_selected <- plot_selected + geom_smooth(alpha = 0.05) 
              
            }
            
          }
 
        }
        
      }
      
      if (!is.null(range_sel)){

        plot_selected <- plot_selected + coord_cartesian(xlim = range_sel$x,
                                                         ylim = range_sel$y)

      }
        
      if (!is.null(input$selected_table_rows_selected) & input$color_var_select == 'USER_NAME'){
        
        plot_selected <- plot_selected +
          geom_point(data = df[as.numeric(input$selected_table_rows_selected),], size = input$psize + 3, aes(color = USER_NAME))
        
        if (input$smooth == 'regression'){
          
          plot_selected <- plot_selected + geom_smooth(data = df[df$USER_NAME %in% unique(df[as.numeric(input$selected_table_rows_selected),'USER_NAME'])$USER_NAME,],
                                                       aes_string(group = 'USER_NAME'), alpha = 0.05, method = 'lm')
       
        } else {
          
          if (input$smooth == 'mean'){
            
            plot_selected <- plot_selected + geom_smooth(data = df[df$USER_NAME %in% unique(df[as.numeric(input$selected_table_rows_selected),'USER_NAME'])$USER_NAME,],
                                                         aes_string(group = 'USER_NAME'), alpha = 0.05)
            
          }
          
        }
        
      } 
      
      
    }
    
  
    
    plot_selected
    
    
  })
  
  # for plot zooming - Selected
  
  range_sel <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$selected_dblclick, {
    
    brush <- input$selected_brush
    
    if (!is.null(brush)){
      
      range_sel$x <- c(brush$xmin, brush$xmax)
      range_sel$y <- c(brush$ymin, brush$ymax)
      
    } else {
      
      range_sel$x <- NULL
      range_sel$y <- NULL 
      
    }
    
  })
  
  output$main_table <- renderRpivotTable({
    
    tab <- rpivotTable(df_util, rows = 'GEO_NAME', cols = 'YEARMONTH', aggregatorName = 'Average', vals = 'util_bill',
                       rendererName = 'Table')
    
    tab
    
  })
  
  plots_clickDF <- reactiveValues(summary = NULL, unit = NULL, YM = NULL, main_dTable = NULL)

  observeEvent(input$main_click, {
    
    df <- dfToPlot$df
    
    YM_levels <- levels(as.factor(df$YEARMONTH))
    YM_clicked <- YM_levels[round(input$main_click$x)]
    
    unit_levelsYM <- levels(as.factor(df[df$YEARMONTH == YM_clicked, input$grouping][[input$grouping]]))
    
    YM_lower_bound <- round(input$main_click$x) - (0.75/2)
    YM_upper_bound <- round(input$main_click$x) + (0.75/2)
    YM_intervals <- seq(from = YM_lower_bound, to = YM_upper_bound, by = 0.75/length(unit_levelsYM))
    
    
    clicked_YM_interval <- cut(input$main_click$x, YM_intervals, include.lowest = T)
    unit_YM_position <- match(as.character(clicked_YM_interval), as.character(levels(clicked_YM_interval)))
    
    clicked_unit <- unit_levelsYM[unit_YM_position]
    
    # for main summary
    
        # summary of unit in YM
    
    summaryDF <- summaryfunction(df[df['YEARMONTH'] == YM_clicked & df[input$grouping] == clicked_unit,  'util_bill'][['util_bill']])
    summaryDF <- gather(summaryDF)
    colnames(summaryDF)[names(summaryDF) == 'value'] <- clicked_unit
    colnames(summaryDF)[names(summaryDF) == 'key'] <- YM_clicked

        # summary of all selected units in YM
    
    summaryDF2 <- summaryfunction(df[df['YEARMONTH'] == YM_clicked,  'util_bill'][['util_bill']])
    summaryDF2 <- gather(summaryDF2)
    summaryDF2$key <- NULL
    colnames(summaryDF2)[names(summaryDF2) == 'value'] <- 'All selected'

    plots_clickDF$summary <- bind_cols(summaryDF, summaryDF2)
    
    # for main datatable
    
    all_unitsDF <- pass_df()
    all_unitsDF <- all_unitsDF[all_unitsDF$YEARMONTH == YM_clicked,] # filter out not clicked months

    all_unitsDF <- all_unitsDF %>%
      group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
                  summarise(min = round(min(util_bill), 2), 
                            max = round(max(util_bill), 2), 
                            avg = round(mean(util_bill), 2),
                            median = round(median(util_bill), 2),
                            t_bill_sum = round(sum(t_bill), 2),
                            t_inv_sum = round(sum(t_inv), 2),
                            count = n()) %>%
                  ungroup()
    
    plots_clickDF$main_dTable <- all_unitsDF
      
    
    
  })
  
  observeEvent(input$marginal2_click, {
    
    df <- dfToPlot$df
    
    unit_levels <- levels(as.factor(df[input$grouping][[input$grouping]]))
    unit_clicked <- unit_levels[round(input$marginal2_click$x)]
    
    # for main summary
    
        # summary of unit
    
    summaryDF <- summaryfunction(df[df[input$grouping] == unit_clicked,  'util_bill'][['util_bill']])
    summaryDF <- gather(summaryDF)
    colnames(summaryDF)[names(summaryDF) == 'value'] <- unit_clicked
    colnames(summaryDF)[names(summaryDF) == 'key'] <- 'All selected months'
    
        # summary of all selected units
    
    summaryDF2 <- summaryfunction(df$util_bill)
    summaryDF2 <- gather(summaryDF2)
    summaryDF2$key <- NULL
    colnames(summaryDF2)[names(summaryDF2) == 'value'] <- 'All selected'
    
    plots_clickDF$summary <- bind_cols(summaryDF, summaryDF2)
    
    # for main datatable
    
    all_unitsDF <- pass_df()

    all_unitsDF <- all_unitsDF %>%
      group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
      summarise(min = round(min(util_bill), 2), 
                max = round(max(util_bill), 2), 
                avg = round(mean(util_bill), 2),
                median = round(median(util_bill), 2),
                t_bill_sum = round(sum(t_bill), 2),
                t_inv_sum = round(sum(t_inv), 2),
                count = n()) %>%
      ungroup()
    
    plots_clickDF$main_dTable <- all_unitsDF
    
  })
  
  output$all_units_statsYM <- renderDataTable({
    
    df <- plots_clickDF$main_dTable
    
    df
    
  })
  
  output$main_summary <- renderTable({
    
    plots_clickDF$summary
    
  })
  
  output$test1 <- renderPrint({
    
    #input$inputs_brush
    #pass_df_selected()[as.numeric(input$selected_table_rows_selected), ]
    #input$inputs_dblclick
    #unique(pass_df_selected_brush()[as.numeric(input$selected_table_rows_selected),'USER_NAME'])
    
  })
  # 
  

  output$hovUserYM <- renderTable({
    
    df <- brushed$df
    
    if (!is.null(input$selected_hover)){
    
      nearPoints(df, maxpoints = 17, input$selected_hover)[, c('USER_NAME', 'YEARMONTH')]
      
    }
    
  })
  
  # users in month selected with brush or with doubleclick in Inputs tab
  
  output$selected_table <- DT::renderDataTable({
    
    df <- brushed$df
    
    if (!is.null(input$inputs_brush) & !is.null(input$units)){
    
      renderDT <- DT::datatable(df) %>%
        formatStyle('util_bill', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue'))) %>%
        formatStyle('t_bill', background = styleColorBar(df$t_bill, 'steelblue'))
      
      renderDT
      
    }
    
    
    
  })
  
  
  output$users_dyg <- renderDygraph({
    
    if (!is.null(input$user_list_select)){
      
      f <- df_util %>% 
        filter(USER_NAME %in% input$user_list_select) %>%
        group_by(USER_NAME, YEARMONTH) %>%
        summarise(t_bill = sum(t_bill), 
                  t_inv = sum(t_inv), 
                  exp_bill = sum(exp_bill)) %>%
        mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
        select(YEARMONTH, USER_NAME, util_bill) %>%
        ungroup()
      
      df_toDygraph <- spread(data = f, key = USER_NAME, value = util_bill)
      
      df_toDygraph_xts <- xts(x = df_toDygraph[, colnames(df_toDygraph) != 'YEARMONTH'], 
                              order.by = as.POSIXct(strptime(as.character(df_toDygraph$YEARMONTH), format = "%Y-%m-%d")))
      
      plot_dyg <- dygraph(df_toDygraph_xts, y = 'User utilization') %>% 
        dyRangeSelector(height = 20) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
                    highlightCircleSize = 4,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = T)
      
      
      
    } else {
      
      plot_dyg <- NULL 
      
    }
    
    plot_dyg
    
  })
  
  output$test2 <- renderPrint({
    
    #clickedCountry()
    
    #userCountry_map()
    
    df <- dataMap$df@data[!is.na(dataMap$df@data$radius), c('name', 'measure_color', 'measure_circle', 'radius', 'maxc')]
    arrange(df, desc(radius))
    
  })
  
  output$test3 <- renderPrint({
    
    #clickedCountry()
    
    #userCountry_map()
    
    dfcirc$df
    
  })
  
  dataMap <- reactiveValues(df = NULL)
  
  observe({
    
    req(input$units, dfToPlot$df, input$map_variable, input$map_statistic, input$map_quant, input$circle_variable)
    
    df <- dfToPlot$df
    
    #if (!is.null(input$units) & input$tabs_1 == 'Map'){
      
      # polygons
      
      df <- df[, c('COUNTRY_NAME', 'YEARMONTH', input$map_variable)]

      if (input$map_statistic == 'mean'){
        
        dfToJoin <- df %>%
          group_by(COUNTRY_NAME) %>%
          summarise_(measure_color = interp(~mean(var, na.rm = T), var = as.name(input$map_variable))) %>%
          ungroup()
        
      } else {
        
        if (input$map_statistic == 'percentile'){
          
          dfToJoin <- df %>%
            group_by(COUNTRY_NAME) %>%
            summarise_(measure_color = interp(~quantile(var, probs = pr, na.rm = T), var = as.name(input$map_variable), pr = input$map_quant)) %>%
            ungroup()
          
          
        }
        
      }
      
      # circles
      
      if (input$circle_variable != 'YM_meanCount'){
        
        df <- dfToPlot$df[, c('COUNTRY_NAME', 'YEARMONTH', input$circle_variable)]
        
        if (input$circle_statistic == 'mean'){
          
          dfToJoin2 <- df %>%
            group_by(COUNTRY_NAME) %>%
            summarise_(measure_circle = interp(~mean(var, na.rm = T), var = as.name(input$circle_variable))) %>%
            ungroup()
          
        } else {
          
          if (input$circle_statistic == 'percentile'){
            
            dfToJoin2 <- df %>%
              group_by(COUNTRY_NAME) %>%
              summarise_(measure_circle = interp(~quantile(var, probs = pr, na.rm = T), var = as.name(input$circle_variable), pr = input$circle_quant)) %>%
              ungroup()
            
          }
          
        }
        
        
        
      } else {
        
        # monthly mean of user count in country
        
        df <- dfToPlot$df[, c('COUNTRY_NAME', 'YEARMONTH')]
        
        dfToJoin2 <- df %>%
          group_by(COUNTRY_NAME) %>%
          summarise(measure_circle = n()/length(unique(df$YEARMONTH))) %>%
          ungroup()
        
      }
      
      
      
      dfToJoin2 <- dfToJoin2 %>%
        mutate(radius = (1000000/max(measure_circle)) * measure_circle) %>%
        mutate(maxc = max(measure_circle))
      #dfToJoin2$maxcountry <- dfToJoin2[dfToJoin2$measure_circle == dfToJoin2$maxc, 'COUNTRY_NAME']$COUNTRY_NAME # moze byt ich aj viac, Error in $<-.data.frame: replacement has 3 rows, data has 5
      
      cat(file=stderr(), "nrow1", nrow(dfToJoin)) 
      cat(file=stderr(), "nrow2", nrow(dfToJoin2)) 
      
      dfToJoin <- dfToJoin %>%
        full_join(dfToJoin2) 
      
      dfToJoin <- rename(dfToJoin, name = COUNTRY_NAME)
      
      # cat(file=stderr(), "measure CAN", dfToJoin[dfToJoin$name == 'Canada','measure_circle'][['measure_circle']]) 
      # cat(file=stderr(), "radius CAN", dfToJoin[dfToJoin$name == 'Canada','radius'][['radius']])
      # cat(file=stderr(), "variable CAN", input$circle_variable)
      
      lnd@data <- lnd@data %>% left_join(dfToJoin)
      
      dataMap$df <- lnd
      
    #} 
    
  })
  
  dfcirc <- reactiveValues(df = NULL)
  
  
  # add polygons and circles
  
  observe({
    
    req(input$map_variable, input$map_statistic, input$map_quant, input$units)
    
    dat <- dataMap$df
    
    proxy <- leafletProxy('countries')
    
    #isolate(
 
    if (!is.null(dat)){
      
      datCircles <- as.data.frame(gCentroid(dat, byid = T))
      datCircles$name <- dat$name
      datCircles$measure_circle <-  dat$measure_circle
      datCircles$measure_color <-  dat$measure_color
      datCircles$radius <- (dat$radius/(abs(datCircles$y)^(2/5))) * 10 # stupid, need adjust circles somehow or reproject somehow, plotted radius differs when changing lat
      datCircles <- datCircles[!is.na(datCircles$measure_circle),]
      
      datCircles <- datCircles %>%
        arrange(desc(radius))
      
      dfcirc$df <- datCircles # for test2 verbatimtext output
      
      
      toShow <- proxy %>%
        clearShapes() %>%
        addPolygons(data = dat, 
                    group = 'poly',
                    color = ~colorpal()(measure_color), 
                    stroke = F, smoothFactor = 0.2, 
                    fillOpacity = 0.4) %>%
        addCircles(data = datCircles,
                   group = 'circ',
                   radius = ~radius,
                   lng = ~x,
                   lat = ~y,
                   weight = 1,
                   color = '#777777',
                   fillColor = '#4c4cff',
                   fillOpacity = 0.3,
                   popup = ~paste(name, ', ', input$circle_variable, ': ',  round(measure_circle, 2), ', ', input$map_variable, ': ', round(measure_color, 2)))
      
       
      
      toShow
    } #)
    
   

  })
  
  
  colorpal <- reactive({
    
    df <- dataMap$df
   
    if (!is.null(df)){
      
      mes <- df@data$measure_color
      
      colorNumeric(palette = heat.colors(6), domain = mes)

    }

  })
  
  # add legend
  
  observe({
    
    df <- dataMap$df
    
    proxy <- leafletProxy('countries')

    if (!is.null(df)){
      
      mes <- df@data$measure_color
      
      proxy %>% 
        clearControls() %>%
        addLegend(position = 'bottomleft',
                          pal = colorpal(),
                          values = mes)
      
    } else {
      proxy %>% clearControls()
    }
    
  })
  
  output$countries <- renderLeaflet({
    
    #req(input$units)
    
    #isolate(
    
    #if (input$tabs_1 == 'Map'){
    
      #pal <- colorpal()
      
      cmap <- leaflet() %>%
        addTiles() %>%
        addLayersControl(
          #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
          overlayGroups = c("poly", "circ"),
          options = layersControlOptions(collapsed = FALSE),
          position = 'bottomright'
        )#)

      cmap
      
    #}
    
    
    
  })
  
  clickedCountry <- reactive({
    
    req(input$countries_shape_click)
    
    lat <- input$countries_shape_click$lat
    lon <- input$countries_shape_click$lng
    coo <- matrix(c(lon, lat), ncol = 2)
    
    ctr <- lnd[SpatialPoints(coords = coo, proj4string = CRS(proj4string(lnd))), ]@data$name
   
    ctr
     
  })
  
  output$usersCountry_table <- DT::renderDataTable({
    
    dat <- dfToPlot$df
    dat[dat$COUNTRY_NAME == clickedCountry(),]
    
  })
  
})

################################################################################

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
  # utilizacia  - podla geo -> dept -> org -> user 41
  #             - datatable mainPlot - pre kazdu unit a yearmonth - median, priemer, 1 kvartil, 3 kvartil, count, percento z viditelneho
  #             - pivotka ked bude viacero metrik, vyber z metrik alebo volba datasetu v promptoch
  #             - pivotka - sortovanie, vyhladavanie - dataTables
  #             - ciara median globalnej utilizacie v mesiaci
  #             - nejako vizualizovat, alebo hodit do tabulky o kolko percent dany subset snizuje/zvysuje median globalnej
  #                 utilizacie v mesiaci a celkovo, mozno aj utilizacie levelu nad, percento hodnot pod medianom
  #             - pre boxplot - checkbox, ci ukazat box pre 1 level nad (pre porovnanie napr. danej organizacie a gea pod ktorym je),
  #                 nebude zahrnuty do marginal plotu
  #             - brushing  bez zoomu ako filter do tabulky pod - utilizacia userov
  #             * slider mesiace aj hodnoty
  #             - druhy graf pod - celkovy histogram alebo boxplot (listbox), podla zvolenych
  #                 centier, dept., userov, zoradeny podla medianov
  #             - marginal plot na to co je zobrazene celkovo (selectnute v promtoch), compedium alebo http://daattali.com/shiny/ggExtra-ggMarginal-demo/
  #                 ako histogram s ciarami vpravo, hore ako barchart (count)
  #             * checkbox na centra, departmenty, alebo userov
  #             - checkbox ci budu prompty kaskadovane
  #             - cislo pri boxplote: expected billable hours
  #             - cislo pri boxplote: rast v % celkovej utilizacie oproti
  #                 minulemu mesiacu
  #             - download csv a grafu

  #             - Main tab - tabulka s uplne vsetkymi units v ramci obdobia a hodnot utilizacie (priemer, median, count) podla mesiacov,
  #             -         - druha tabulka s celkovo za vsetky zvolene mesiace


# analyticka funkcionalita - rozne metriky na task, round?
  # 1 premenna - grafy, summary
  # 2 a viac premennych - korelacna matica,
  # zistovat pravdepodobnost roznych eventov - markove modely, asociacne pravidla?
  # analyza workflov
  # analyza 'zdrojovej narocnosti' taskov
  # moznost analyzovat vlastny datovy set
# vizualizacne vychytavky - rCharts, Google charts (google vis), plotly, ggvis, ggplot2
  # interaktivna mapa, nejaka jednoducha metrika na staty
  # googleVis - calendar chart - napr. pocet submitnutych taskov na osobu, centrum, sponsora
  #   - motion chart - s animaciou
  #   - sankey diagram - budget a spending projektov?
  # radar chart - rozne metriky na geo, center, dpt, usera / spiderweb chart - budget vs. spending http://jkunst.com/highcharter/highcharts.html
  # mosaic plot/ treemap


# v tabulke user je v 34 riadku volaka cinska picovina, robi bordel - stlpec BUSNEED
  
# filtre - date_range, util_value, reac_units -> grouping - grouping -> union - check_uplevel

# MAPA  - datum, utilizacia filter -> zakl. data do leafletu na vykreslenie prazdnych polygonov ##### NETREBA???
#       - unit filter do observera -> leafletproxy na prekreslovanie
#       - 

# prezentacia

# 1. ci chapu pojmy
# 2. oboznamenie - najprv R a Shiny, potom apka
# 3. R - programovaci jayzyk zamerany na pracu s datami, opensource, rozsiritelny pomocou kniznic/packegov, RStudio - ukazat aj trocha z kodu
#       - najst nejake cisla porovnania s inymi analytickymi toolmi, cisla zo stackoverflow, podpora pre najvacsie cloudove/analyticke platformy
#       - t.j. - spracovanie dat z roznych zdrojov
#               - manipulacia s datami - vektory, matice, dataframy...principy, kniznice (sqldf, reshape, dplyr, tidyr
#               - analyza (kopec uz vytvorenych funkcii a kopec dalsich moznych kniznic) a vizualizacia dat (uz vytvorene base funkcie
#                       plus ggplot2 (strucne opisat), ggvis, plotly, dygraph, D3, google charts, rCharts, geograficke data leaflet, tmap a dalsie,
#                       interaktivita, animacie
#               - modelovanie dat - vsemozne statisticke algoritmy, strojoveho ucenia, clustrovanie, segmentacne, asociacne,
#                                       - modelovanie streamovanych dat (zive?), sluzby zez api
#               - dalsie...
# 4. shiny - kniznica/web application framework pre R (tj. v R spravis vsetku pracu s datami (manipulovanie, modelovanie, analyzy...) a v Shiny riesis funkcionalitu)
#           - koncept reaktivity
#           - moznost interaktivity podla pouzitej kniznice (grafy, tabulky)
# 5. moja apka - popisat UIcko - panel na prompty, ktore sa dynamicky menia a taby
#               - popisat pri kazdom vystupe ake informacie ukazuje a moznosti zlepsenia (napr. pri boxplote violin plot, zobrazit cislo s nejakou inou premennou, 
#                       alebo klik na nejaku katogoriu a zobrazenie nejakeho ineho grafu ci tabulky)
#               - popisat cca ako to funguje
# 6. co dalsie by sa malo dat este v shiny spravit, priklady, javascript css html v kode
# 7. R + Shiny ako tool na reporting?  - dashboardy, shiny server a shinyapps.io, 
#                                     - parametrizovane a interaktivne HTML (PDF, WORD,...) dokumenty a prezentacie (slidy, slidify, ioslides)
#                                         vytvorene v RMarkdown, automatizacia?, encoding dat (RData) do html, HTML report
#                                         v Shiny appke/dashboarde alebo samostatne (mesacna, kentova, utilizacia od vila, personalizovany
#                                         report s roznymi metrikami a ich spracovanim
#                                     - nutne preskumat nevyhnutnu funkcionalitu
#                                     - crosstaby jednoducho?
# 8. Strucne zhrnut pozitiva a negativa a veci co este treba preskumat
