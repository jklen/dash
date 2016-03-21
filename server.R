
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(ggExtra)
library(gridExtra)
library(lubridate)

# data load

load('.Rdata')

shinyServer(function(input, output, session) {
  
  # rendering listbox for histogram marginal plot (overlaying or stacked)
  
  output$marg_cat_hist_type <- renderUI({
    
    if (input$marginal_vis == 'Histograms'){
      
      selectInput(inputId = 'marg_type',
                  label = NULL,
                  choices = c('Overlaying',
                              'Stacked'),
                  selected = 'Stacked'
      )
      
    }
    
  })
  
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
  
  # creating reactive vector of units based on selected groupings
  
  pass_units <- reactive({
    
    if (input$grouping == 'GEO_NAME'){
      units <- unique(df_util$GEO_NAME)
    } else {
      
      if (input$grouping == 'ORG_NAME'){
        units <- unique(df_util$ORG_NAME) 
      } else {
        
        if (input$grouping == 'DEPT_NAME'){
          units <- unique(df_util$DEPT_NAME) 
        } else {
          units <- unique(df_util$USER_NAME) 
        }
      }
    }
    
    #cat(file=stderr(), "selected grouping - ", input$grouping) # vypise do konzoly

  units  
    
  })
  
  # creating reactive dataframe based on date, value, selected units
  
  pass_df_util <- reactive({
    
    df_util_reac <- df_util[between(df_util$YEARMONTH, as.POSIXct(input$date_range[1]) - days(1), as.POSIXct(input$date_range[2])),]
    
    df_util_reac <- df_util_reac[between(df_util_reac$util_bill, input$util_value[1], input$util_value[2]),]

    
    # cat(file=stderr(), " checkbox items - ", input$units)
    # cat(file=stderr(), " checkbox null?? - ", is.null(input$units))
    # cat(file=stderr(), "  df rows - ", nrow(df_util_reac))
    
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
    
    #cat(file=stderr(), " chart input class - ", class(df_util_reac))
    
    df_util_reac <- df_util_reac %>% 
      group_by_(.dots = lapply(groups_toPlot, as.symbol)) %>%
      summarise(t_bill = sum(t_bill), 
                t_inv = sum(t_inv), 
                exp_bill = sum(exp_bill)) %>%
      mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
      ungroup()
    
    #cat(file=stderr(), " rows of df for chart input ", nrow(df_util_reac))
    
    # if selected, append data from level above (except for geo)
    
    df_util_reac
    
  })
  
  # plot to output
  
  output$utilization_YM <- renderPlot({
    
    if (!is.null(input$units)){
      
      if (input$grouping != 'USER_NAME'){
      
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
        
        plot_util_YM <- ggplot(aes(x = YEARMONTH, y = util_bill, color = USER_NAME), data = pass_df_util()) +
          geom_line()
      
      }
      
      plot_util_YM
      
    }
    
  })
  
  output$Utilization_marginal1 <- renderPlot({
    
    if (!is.null(input$units)){
    
      plot_util_marg1 <- ggplot(aes(x = util_bill), data = pass_df_util()) +
        geom_histogram(fill = '#F79420', color = 'black') +
        geom_vline(xintercept = mean(pass_df_util()$util_bill)) +
        coord_flip() +
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
              axis.line = element_line(colour = '#BDBDBD'))
    
    plot_util_marg1
    
    }
    
  })
  
  output$Utilization_marginal2 <- renderPlot({
    
    if (!is.null(input$units)){
      
      if (input$marginal_vis == 'Boxplots'){
        
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
                axis.line = element_line(colour = '#BDBDBD'))
        
        plot_util_marg2
        
      
      }else {
        
        if (input$marginal_vis == 'Histograms'){
        
          if (input$marg_type == 'Overlaying'){
            
            plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = input$grouping), data = pass_df_util()) +
              geom_histogram(alpha = 0.4, position = 'identity') + 
              coord_flip() +
              theme(legend.position = 'none',
                    panel.background = element_rect(fill =NA),
                    panel.grid.major = element_line(colour = '#F6F6F6'),
                    axis.line = element_line(colour = '#BDBDBD'))
            
            plot_util_marg2
            
          }else {
            
            if (input$marg_type == 'Stacked'){
              
              plot_util_marg2 <- ggplot(aes_string(x = 'util_bill', fill = input$grouping), data = pass_df_util()) +
                geom_histogram() +
                coord_flip() +
                theme(legend.position = 'none',
                      panel.background = element_rect(fill =NA),
                      panel.grid.major = element_line(colour = '#F6F6F6'),
                      axis.line = element_line(colour = '#BDBDBD'))
              
              plot_util_marg2
              
            }
            
          }
          
        }
        
      }
      
    }
    
  })
  
  output$clicked <- renderPrint ({
    
    input$util_YM_click
    #cat(file=stderr(), " -------", input$marg_cat_hist_type)
    
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
  #             - hlavny graf - boxplot
  #             * namiesto checkboxov pre dpt a usera hodit to s vyhladavanim
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
  # upload suboru
# vizualizacne vychytavky - rCharts, Google charts (google vis), plotly, ggvis, ggplot2
  # interaktivna mapa, nejaka jednoducha metrika na staty
  # googleVis - calendar chart - napr. pocet submitnutych taskov na osobu, centrum, sponsora
  #   - motion chart - s animaciou
  #   - sankey diagram - budget a spending projektov?
  # radar chart - rozne metriky na geo, center, dpt, usera / spiderweb chart - budget vs. spending http://jkunst.com/highcharter/highcharts.html
  # mosaic plot/ treemap


# v tabulke user je v 34 riadku volaka cinska picovina, robi bordel - stlpec BUSNEED
  
# filtre - date_range, util_value, reac_units -> grouping - grouping -> union - check_uplevel