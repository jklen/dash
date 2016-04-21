
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

# data load

load('.Rdata')

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
  
  output$util_input_selected <- renderUI({
    
    selectInput(inputId = 'util_inputs_selected',
                choices = c('Tracked billable' = 't_bill',
                            'Expected billable' = 'exp_bill',
                            'Tracked investment' = 't_inv'),
                label = 'Y variable',
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
                                    DPT_NAME = c('Department' = 'DPT_NAME')),
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
  
  # creating reactive dataframe based on date and value (units in prompt updated)
  
  pass_df <- reactive({
   
    df_util_r <- df_util[between(df_util$YEARMONTH, as.POSIXct(input$date_range[1]) - days(1), as.POSIXct(input$date_range[2])),]
    df_util_r <- df_util_r[between(df_util_r$util_bill, input$util_value[1], input$util_value[2]),]
    
    df_util_r$YEARMONTH <- as.factor(df_util_r$YEARMONTH)
    
    df_util_r
    
  })
  
  # creating reactive dataframe based on selected units
  
  pass_df_util <- reactive({
   
    df_util_reac <- pass_df()
    
    if(!is.null(input$units)){
      
      if (input$grouping == 'GEO_NAME'){

        df_util_reac <- df_util_reac[df_util_reac$GEO_NAME %in% input$units,]

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
      
    }
    
    # group reactive dataframe based on selected grouping
    
    if (input$grouping == 'USER_NAME'){
      
      groups_toPlot <- c('YEARMONTH', 'USER_NAME')
      
    } else {
      
      groups_toPlot <- c(input$grouping, 'YEARMONTH', 'USER_NAME') 
      
    }

    df_util_reac <- df_util_reac %>% 
      group_by_(.dots = lapply(groups_toPlot, as.symbol)) %>%
      summarise(t_bill = sum(t_bill), 
                t_inv = sum(t_inv), 
                exp_bill = sum(exp_bill)) %>%
      mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
      ungroup()
    
    df_util_reac
    
  })
  
  
  # for plot zooming
  
  range <- reactiveValues(y = NULL)
  
  observeEvent(input$marginal1_dblclick, {
    
    brush <- input$marginal1_brush
    
    if (!is.null(brush)){
      
      range$y <- c(brush$ymin, brush$ymax)
      
    } else {
      
      range$y <- NULL 
      
    }
    
  })


  # plot to output
  
  output$utilization_YM <- renderPlot({
    
    if (!is.null(input$units)){
      
      if (input$grouping != 'USER_NAME'){
        
        if (input$mainPlotVis == 'Boxplots'){
      
          plot_util_YM <- ggplot(aes_string(y = 'util_bill', fill = input$grouping), data = pass_df_util()) + 
            geom_boxplot(aes(x = factor(YEARMONTH))) +
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
                  panel.grid.major = element_line(colour = '#F6F6F6'),
                  axis.line = element_line(colour = '#BDBDBD'))
        
        } else {
         
          if (input$mainPlotVis == 'Stacked barchart with counts') {
            
            plot_util_YM <- ggplot(aes_string(fill = input$grouping), data = pass_df_util()) +
              geom_bar(aes(x = factor(YEARMONTH)), position = 'stack') +
              theme(panel.background = element_rect(fill =NA),
                    panel.grid.major = element_line(colour = '#F6F6F6'),
                    axis.line = element_line(colour = '#BDBDBD'))
            
          } else {
            
            if (input$mainPlotVis == 'Stacked relative barchart'){
              
              plot_util_YM <- ggplot(aes_string(fill = input$grouping), data = pass_df_util()) +
                geom_bar(aes(x = factor(YEARMONTH)), position = 'fill') +
                theme(panel.background = element_rect(fill =NA),
                      panel.grid.major = element_line(colour = '#F6F6F6'),
                      axis.line = element_line(colour = '#BDBDBD'))
              
            } else {
              
              if (input$mainPlotVis == 'Dodged barchart'){
                
                plot_util_YM <- ggplot(aes_string(fill = input$grouping), data = pass_df_util()) +
                  geom_bar(aes(x = factor(YEARMONTH)),position = 'dodge') +
                  theme(panel.background = element_rect(fill =NA),
                        panel.grid.major = element_line(colour = '#F6F6F6'),
                        axis.line = element_line(colour = '#BDBDBD')) 
                
              }
              
            }
            
          }
          
        }
        
        
      } else {
        
        
        
         # plot_util_YM <- ggplot(aes(x = YEARMONTH, y = util_bill, color = USER_NAME), data = pass_df_util()) +
         #   geom_line()
      
      }
      
      if (!is.null(range) & input$mainPlotVis == 'Boxplots'){
        
        plot_util_YM <- plot_util_YM + coord_cartesian(ylim = range$y)
        
      }
      
      plot_util_YM
      
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
  
  
  output$Utilization_marginal1 <- renderPlot({
    
    if (!is.null(input$units)){
    
      plot_util_marg1 <- ggplot(aes(x = util_bill), data = pass_df_util()) +
        geom_histogram(fill = '#F79420', color = 'black') +
        geom_vline(xintercept = mean(pass_df_util()$util_bill)) +
        geom_vline(xintercept = as.numeric(mean(pass_df_util()$util_bill, na.rm = T)),
                   color = 'red') +
        geom_vline(xintercept = as.numeric(median(pass_df_util()$util_bill, na.rm = T)),
                   color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(pass_df_util()$util_bill,
                                                    probs = 0.25, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(pass_df_util()$util_bill,
                                                    probs = 0.75, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(pass_df_util()$util_bill,
                                                    probs = 0.1, na.rm = T)),
                   linetype = 3) +
        geom_vline(xintercept = as.numeric(quantile(pass_df_util()$util_bill,
                                                    probs = 0.9, na.rm = T)),
                   linetype = 3) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#F6F6F6'),
              axis.line = element_line(colour = '#BDBDBD'),
              axis.title.y = element_blank())
    
      if (!is.null(range)){
        
        plot_util_marg1 <- plot_util_marg1 + coord_flip(xlim = range$y)
        
      }
      
      plot_util_marg1
     
    }
    
  })
  
  output$Utilization_marginal2 <- renderPlot({
    
    if (!is.null(input$units)){
      
      if (input$marginalVis == 'Boxplots'){
        
        plot_util_marg2 <- ggplot(aes_string(y = 'util_bill', x = input$grouping, fill = input$grouping), data = pass_df_util()) +
          geom_boxplot() +
          geom_point(fun.y = mean, stat = 'summary', shape = 1) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.1),
                     stat = 'summary', shape = 4) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.9),
                     stat = 'summary', shape = 4) +
          theme(legend.position = 'none',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#F6F6F6'),
                axis.line = element_line(colour = '#BDBDBD'),
                axis.title.y = element_blank())
        
        if (!is.null(range)){
          
          plot_util_marg2 <- plot_util_marg2 + coord_cartesian(ylim = range$y)
          
        }
        
        plot_util_marg2
        
      
      } else {
        
        if (input$marginalVis == 'Overlaid histograms'){

            plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = input$grouping), data = pass_df_util()) +
            geom_histogram(alpha = 0.4, position = 'identity') + 
            theme(legend.position = 'none',
                  panel.background = element_rect(fill =NA),
                  panel.grid.major = element_line(colour = '#F6F6F6'),
                  axis.line = element_line(colour = '#BDBDBD'),
                  axis.title.y = element_blank())
            
            if (!is.null(range)){
              
              plot_util_marg2 <- plot_util_marg2 + coord_flip(xlim = range$y)
              
            }
            
            plot_util_marg2
            
        } else {
            
          if (input$marginalVis == 'Stacked histograms'){
            
            plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = input$grouping), data = pass_df_util()) +
              geom_histogram() +
              theme(legend.position = 'none',
                    panel.background = element_rect(fill =NA),
                    panel.grid.major = element_line(colour = '#F6F6F6'),
                    axis.line = element_line(colour = '#BDBDBD'),
                    axis.title.y = element_blank())
            
            if (!is.null(range)){
              
              plot_util_marg2 <- plot_util_marg2 + coord_flip(xlim = range$y)
              
            }
            
            plot_util_marg2
              
            
            
          } else {
            
            if (input$marginalVis == 'Stacked relative barchart'){
              
              plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = input$grouping), data = pass_df_util()) +
                geom_histogram(position = 'fill') +
                ylab('%') +
                theme(legend.position = 'none',
                      panel.background = element_rect(fill =NA),
                      panel.grid.major = element_line(colour = '#F6F6F6'),
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
  
  output$main_table <- renderRpivotTable({
    
    tab <- rpivotTable(df_util, rows = 'GEO_NAME', cols = 'YEARMONTH', aggregatorName = 'Average', vals = 'util_bill',
                       rendererName = 'Table')
    
    tab
    
  })
  
  output$main_summary <- renderPrint ({
    
    if (!is.null(input$units)){
      
      by(data = pass_df_util()$util_bill, INDICES = pass_df_util()[, c(input$grouping, 'YEARMONTH')], summary)
      
    }
    
    
    
  })
  
  output$marginal_summary <- renderPrint({
    
    if (!is.null(input$units)){
      
      summary(pass_df_util()$util_bill)
      
    }
    
  })
  
  output$marginal_summary2 <- renderPrint({
    
    if (!is.null(input$units)){
      
      by(data = pass_df_util()$util_bill, INDICES = pass_df_util()[, input$grouping], summary)

    }
    
  })
  
  output$test <- renderPrint({
    
    input$marginal1_brush
    #range$y
    #str(input$marginal1_brush)
    #str(input$mainPlot_brush)
    
  })
  
  output$test1 <- renderPrint({
    
    #input$inputs_brush
    #pass_df_selected()[as.numeric(input$selected_table_rows_selected), ]
    #input$inputs_dblclick
    unique(pass_df_selected_brush()[as.numeric(input$selected_table_rows_selected),'USER_NAME'])
    
  })
  
  # inputs tab plot
  

  output$utilization_inputs <- renderPlot({
    
    if (!is.null(input$units)){
      
      if (input$color_var == 'none'){
    
        plot_util_rel <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs), data = pass_df_util()) +
          geom_point(alpha = 1/input$alpha, position = 'jitter') +
          facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)), margins = T) +
          theme(#aspect.ratio  = 1, - bug pri brushingu
                legend.position = 'none',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#F6F6F6'),
                axis.line = element_line(colour = '#BDBDBD'),
                strip.background = element_rect(fill = '#e5e5ff'),
                strip.text = element_text(face = 'bold'))
      
      } else {
      
        if (input$color_var != 'none'){
          
          plot_util_rel <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs, color = input$color_var), data = pass_df_util()) +
            geom_point(alpha = 1/input$alpha, position = 'jitter') +
            facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)), margins = T) +
            scale_colour_gradientn(colours=rainbow(5)) +
            theme(#aspect.ratio  = 1, - bug pri brushingu
              #legend.position = 'none',
              panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#F6F6F6'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold')) 
          
        }
      }
      
      if (input$smooth == T){
        
        plot_util_rel <- plot_util_rel + geom_smooth(method = 'lm', alpha = 0.05) 
        
      }
      
      plot_util_rel
    
    }
    
  })
  
  # chart in Selected tab
  
  output$selected_chart <- renderPlot ({
    
    if (!is.null(input$inputs_brush)){
      
      # plot_selected <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs), 
      #                         data = pass_df_selected_brush()) +
      #   theme(#legend.position = 'none',
      #     panel.background = element_rect(fill =NA),
      #     panel.grid.major = element_line(colour = '#F6F6F6'),
      #     axis.line = element_line(colour = '#BDBDBD'),
      #     strip.background = element_rect(fill = '#e5e5ff'),
      #     strip.text = element_text(face = 'bold'))
      
      if (input$color_var_select %in% c('none', 'USER_NAME')){
        
        plot_selected <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs_selected),
                                data = pass_df_selected_brush()) +
          geom_point(alpha = 1/input$alpha, position = 'jitter') +
          theme(#legend.position = 'none',
            panel.background = element_rect(fill =NA),
            panel.grid.major = element_line(colour = '#F6F6F6'),
            axis.line = element_line(colour = '#BDBDBD'),
            strip.background = element_rect(fill = '#e5e5ff'),
            strip.text = element_text(face = 'bold'))
        
        #plot_selected <- plot_selected + geom_point(alpha = 1/input$alpha, position = 'jitter')
        
      } else {
      
        if (input$color_var_select != input$grouping){
          
          plot_selected <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs_selected, color = input$color_var_select),
                                  data = pass_df_selected_brush()) +
            geom_point(alpha = 1/input$alpha, position = 'jitter') +
            scale_colour_gradientn(colours = rainbow(5)) +
            theme(#legend.position = 'none',
              panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#F6F6F6'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold'))
          
  
          # plot_selected <- plot_selected + 
          #   geom_point(aes_string(color = input$color_var_select), alpha = 1/input$alpha, position = 'jitter') +
          #   scale_colour_gradientn(colours = rainbow(5))
  
        } else {
          
          if (input$color_var_select == input$grouping){
            
            plot_selected <- ggplot(aes_string(x = 'util_bill', y = input$util_inputs_selected, color = input$color_var_select),
                                    data = pass_df_selected_brush()) +
              geom_point(alpha = 1/input$alpha, position = 'jitter') +
              theme(#legend.position = 'none',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#F6F6F6'),
                axis.line = element_line(colour = '#BDBDBD'),
                strip.background = element_rect(fill = '#e5e5ff'),
                strip.text = element_text(face = 'bold'))
          
            # plot_selected <- plot_selected + geom_point(aes_string(color = input$color_var_select), alpha = 1/input$alpha, position = 'jitter')
          
          }
          
        }
      }
      
      if (input$smooth == T) {
        
        if (input$color_var_select == input$grouping){
          
          plot_selected <- plot_selected + geom_smooth(method = 'lm', aes_string(group = input$grouping), alpha = 0.05) 
          
        } else {
          
          if (input$color_var_select != 'USER_NAME'){
            
            plot_selected <- plot_selected + geom_smooth(method = 'lm', alpha = 0.05) 
            
          }
          
        }
        
        #plot_selected <- plot_selected + geom_smooth(method = 'lm') 
        
      }
      
      if (!is.null(input$selected_table_rows_selected) & input$color_var_select == 'USER_NAME'){
        
        plot_selected <- plot_selected +
          geom_point(data = pass_df_selected_brush()[as.numeric(input$selected_table_rows_selected),], size = 5, aes(color = USER_NAME))
        
        if (input$smooth == T){
          
          plot_selected <- plot_selected + geom_smooth(data = pass_df_selected_brush()[pass_df_selected_brush()$USER_NAME %in% unique(pass_df_selected_brush()[as.numeric(input$selected_table_rows_selected),'USER_NAME'])$USER_NAME,],
                                                       aes_string(group = 'USER_NAME'), alpha = 0.05, method = 'lm')
          
          #cat(file=stderr(), "UNIQUE", unique(pass_df_selected_brush()[as.numeric(input$selected_table_rows_selected),'USER_NAME'])) 
          
        }
        
      } 
      
    plot_selected
      
    }
    
    
    
  })
  

  # reactive dataframe based on dblcicked or brushed in Inputs tab, goes to chart and table in Select tab
  
  pass_df_selected_brush <- reactive ({
    
    if (!is.null(input$units)){
    
      if (!is.null(input$inputs_brush)){
        
        if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 != '(all)'){
          
          selected_brush <- pass_df_util()[pass_df_util()$YEARMONTH == input$inputs_brush$panelvar2 &
                                       pass_df_util()$util_bill >= input$inputs_brush$xmin &
                                       pass_df_util()$util_bill <= input$inputs_brush$xmax &
                                       pass_df_util()[input$util_inputs] >= input$inputs_brush$ymin &
                                       pass_df_util()[input$util_inputs] <= input$inputs_brush$ymax, ]
          
        } else {
          
          if (input$inputs_brush$panelvar1 != '(all)' & input$inputs_brush$panelvar2 == '(all)'){
            
            selected_brush <- pass_df_util()[pass_df_util()[input$grouping] == input$inputs_brush$panelvar1 &
                                         pass_df_util()$util_bill >= input$inputs_brush$xmin &
                                         pass_df_util()$util_bill <= input$inputs_brush$xmax &
                                         pass_df_util()[input$util_inputs] >= input$inputs_brush$ymin &
                                         pass_df_util()[input$util_inputs] <= input$inputs_brush$ymax, ] 
            
          } else {
            
            if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 == '(all)'){
              
              selected_brush <- pass_df_util()[pass_df_util()$util_bill >= input$inputs_brush$xmin &
                                           pass_df_util()$util_bill <= input$inputs_brush$xmax &
                                           pass_df_util()[input$util_inputs] >= input$inputs_brush$ymin &
                                           pass_df_util()[input$util_inputs] <= input$inputs_brush$ymax, ] 
              
            } else {
              
              selected_brush <- brushedPoints(pass_df_util(), 
                                        input$inputs_brush, 
                                        'util_bill', 
                                        input$util_inputs)
              
            }
          }
          
        }
        
        
        
        #selected
        
      }
      
    }
    
    
    
  })
  
  # users in month selected with brush or with doubleclick in Inputs tab
  
  output$selected_table <- DT::renderDataTable({
    
    if (!is.null(input$inputs_brush)){
    
      renderDT <- DT::datatable(pass_df_selected_brush()) %>%
        formatStyle('util_bill', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue'))) %>%
        formatStyle('t_bill', background = styleColorBar(pass_df_selected_brush()$t_bill, 'steelblue'))
      
      renderDT
      
    }
    
    
    
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