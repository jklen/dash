
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
library(timevis)

# data load

load('.RData')

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

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

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
  
  # merging units
  
  output$mergeUI <- renderUI({
    
    req(input$units)
    
    if (is.null(mergedCats$sep)){
    
      units_toMerge <- input$units
      
    } else {
      
      units_toMerge <- input$units[!(input$units %in% mergedCats$sep)]
      
    }
    
    selectInput('merge',
                label = 'Merge',
                selectize = T,
                multiple = T,
                choices = units_toMerge
    )
    
  })
  
  output$mergeButtonUI <- renderUI({
    
    req(input$merge)
    
    if (length(input$merge) >= 2){
    
      actionButton('mergeButton',
                   label = 'Merge',
                   width = '70px'
      )
      
    }
    
  })
  
  observeEvent(c(input$grouping, input$date_range, input$util_value), {
    
    mergedCats$merg = NULL
    mergedCats$sep = NULL
    mergedCats$choice = NULL
    
  })
  
  output$mergeButtonNULLUI <- renderUI({
    
    if (!is.null(mergedCats$merg) & is.null(input$merge)){
      
      actionButton('mergeButtonNULL',
                   label = 'Split',
                   width = '70px'
      )
      
    }
    
  })
  
  observeEvent(input$mergeButtonNULL, {
    
    mergedCats$merg = NULL
    mergedCats$sep = NULL
    mergedCats$choice = input$units
    
  })
 
  mergedCats <- reactiveValues(merg = NULL, sep = NULL, choice = NULL)
  
  observeEvent(input$mergeButton, {
    
    units <- input$units

    mergedCats$sep <- unique(c(mergedCats$sep, input$merge))
    mergedCats$merg <- c(as.list(mergedCats$merg), list(input$merge))
    mergedCats$choice <- units[!(units %in% mergedCats$sep)]
    

    
  })
  
  observe({
    
    req(mergedCats$choice)
    
    ch <- mergedCats$choice
    
    #cat(file=stderr(), "selected grouping - ", ch)
    
    updateSelectInput(session,
                      'merge',
                      choices = ch,
                      selected = character(0))
    
  })
  
  # rendering listbox for possible levels in Influence tab
  
  output$levelUI <- renderUI({
    
    l <- c('Global' = 'global',
           'Geo' = 'GEO_NAME',
           'Organization' = 'ORG_NAME',
           'Department' = 'DEPT_NAME')
    
    l <- l[1:match(input$grouping, l) - 1]
    
    selectInput('level',
                   label = 'Level',
                   choices = l,
                   multiple = F)
    
  })
  
  # rendering listbox for hiding certain units in chosen level
  
  output$influence_hideUI <- renderUI({
    
    req(influenceDF$mainPlot, input$units)
    
    df <- influenceDF$mainPlot
    
    if (input$level != 'global'){
    
      units <- unique(df[[input$level]])
      
      selectInput('influence_hide',
                  label = 'Hide',
                  choices = units,
                  selectize = T,
                  multiple = T)
      
    }
    
    
    
  })
  
  output$filterOutUI <- renderUI({
    
    req(input$level)
    

    l <- c('Global' = 'global',
           'Geo' = 'GEO_NAME',
           'Organization' = 'ORG_NAME',
           'Department' = 'DEPT_NAME',
           'User' = 'USER_NAME')
    
    l <- l[(match(input$level, l) + 1):length(l)]
    l <- c('None' = 'none', l)
    
    selectInput('filterOut',
                label = 'Filter out',
                choices = l,
                multiple = F,
                selected = 'none')
    
  })
  
  output$filterOutUnitUI <- renderUI({
    
    req(input$filterOut)
    
    df <- pass_df()
    
    if (input$filterOut != 'none'){
    
      if (is.null(input$units)){
        
        funits <- unique(df[[input$filterOut]])
        
      } else {
        
        funits <- unique(df[!(df[[input$filterOut]] %in% input$units),][[input$filterOut]])
        
      }
      
      selectInput('filterOutUnit',
                  label = NULL,
                  choices = funits,
                  multiple = T,
                  selectize = T)
    
    }  
      
  })
  
 
  
  # rendering listbox of possible color variables in Inputs tab
  
  output$util_input_color <- renderUI({
    
    color_var_list <- pass_color_var()
    
    selectInput(inputId = 'color_var',
                label = 'Color variable',
                choices = c(color_var_list, 'None' = 'none'),
                selected = 'var_ti',
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
                choices = c('Var_tb' = 'var_tb',
                            'Var_eb' = 'var_eb',
                            'Var_ti' = 'var_ti'),
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
    
    color_vars <- c('Var_tb' = 'var_tb',
                    'Var_eb' = 'var_eb',
                    'Var_ti' = 'var_ti')
    
    color_vars <- color_vars[color_vars != input$util_inputs]
    
    color_vars
    
  })
  
  pass_color_var_selected <- reactive({
    
    color_vars_selected <- c('Var_tb' = 'var_tb',
                             'Var_eb' = 'var_eb',
                             'Var_ti' = 'var_ti',
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
    
    vars <- c('Var_tb' = 'var_tb',
              'Var_eb' = 'var_eb',
              'Var_ti' = 'var_ti')
    
    vars <- vars[vars != input$three_x_var]
    
    selectInput(inputId = 'three_y_var',
                label = 'Y variable',
                choices = vars,
                selected = 'var_ti',
                multiple = F,
                width = '200px')
    
  })
  
  output$three_color <- renderUI({
    
    vars <- c('Var_tb' = 'var_tb',
              'Var_eb' = 'var_eb',
              'Var_ti' = 'var_ti',
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
    
    vars <- c('Var_tb' = 'var_tb',
              'Var_eb' = 'var_eb',
              'Var_ti' = 'var_ti')
    
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
    
    vars <- c('Utilization' = 'var_ub',
              'Var_tb' = 'var_tb',
              'Var_eb' = 'var_eb',
              'Var_ti' = 'var_ti',
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
    df_util_r <- df_util_r[between(df_util_r$var_ub, input$util_value[1], input$util_value[2]),]
    
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
    
    # if (!is.null(df_util_reac)){
    #   
    #   df_util_reac <- df_util_reac %>% 
    #     group_by_(.dots = lapply(groups_toPlot, as.symbol)) %>%
    #     summarise(var_tb = sum(var_tb), 
    #               var_ti = sum(var_ti), 
    #               var_eb = sum(var_eb)) %>%
    #     mutate(var_ub = (var_tb + var_ti)/var_eb) %>%
    #     ungroup()
    # }
    # 
    # df_util_reac$var_tb <- range01(df_util_reac$var_tb) * 100
    # df_util_reac$var_ti <- range01(df_util_reac$var_ti) * 100
    # df_util_reac$var_ub <- range01(df_util_reac$var_ub)
    
    dfToPlot$df <- df_util_reac
    
    
  })
  
  brushed <- reactiveValues(df = NULL)
  
  observeEvent(input$inputs_brush, {
    
    req(input$units) # because change in grouping => nullify units => error
    
    df <- dfToPlot$df
    
    isolate(
      
      if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 != '(all)'){
        
        brushed$df <- df[df$YEARMONTH == input$inputs_brush$panelvar2 &
                           df$var_ub >= input$inputs_brush$ymin &
                           df$var_ub <= input$inputs_brush$ymax &
                           df[input$util_inputs] >= input$inputs_brush$xmin &
                           df[input$util_inputs] <= input$inputs_brush$xmax, ]
        
      } else {
        
        if (input$inputs_brush$panelvar1 != '(all)' & input$inputs_brush$panelvar2 == '(all)'){
          
          brushed$df <- df[df[input$grouping] == input$inputs_brush$panelvar1 &
                             df$var_ub >= input$inputs_brush$ymin &
                             df$var_ub <= input$inputs_brush$ymax &
                             df[input$util_inputs] >= input$inputs_brush$xmin &
                             df[input$util_inputs] <= input$inputs_brush$xmax, ] 
          
        } else {
          
          if (input$inputs_brush$panelvar1 == '(all)' & input$inputs_brush$panelvar2 == '(all)'){
            
            brushed$df <- df[df$var_ub >= input$inputs_brush$ymin &
                               df$var_ub <= input$inputs_brush$ymax &
                               df[input$util_inputs] >= input$inputs_brush$xmin &
                               df[input$util_inputs] <= input$inputs_brush$xmax, ] 
            
          } else {
            
            brushed$df <- brushedPoints(df, 
                                        input$inputs_brush,
                                        input$util_inputs,
                                        'var_ub')
            
          }
        }
        
      }
      
    )
    #selected_brush
    
  })
  
  influenceDF <- reactiveValues(mainPlot = NULL, margPlot = NULL, testdf = NULL, testdf2 = NULL)
  
  output$test_influence <- renderPrint({
    t <- data.frame(influenceDF$testdf)
    # t <- t[!is.na(t$statMov) & !is.nan(t$statMov) & !is.infinite(t$statMov), ]
    
    t
  })
  
  output$test_influence_overall <- renderPrint({
    
    t <- data.frame(influenceDF$testdf2)
    
    t
    
  })
  
  observeEvent(c(input$units, input$influenceOpts, input$influence_choice, input$influenceQuantile, input$level, input$filterOut, input$filterOutUnit, mergedCats$merg), {
    
    req(dfToPlot, input$units, input$filterOut)
    
    # df <- dfToPlot$df
    dfAll <- pass_df()
    
    # merging units in dataframe, and set units for calculations
    
    if (!is.null(mergedCats$merg)){
      
      for (unitToMerge in mergedCats$merg){
        
        dfAll[[input$grouping]] <- ifelse(dfAll[[input$grouping]] %in% unitToMerge,
                                        paste(unitToMerge, collapse = '-'),
                                        dfAll[[input$grouping]])
        
        # cat(file=stderr(), "unit to merge - ", unitToMerge)
        # cat(file=stderr(), "units pasted - ", paste(unitToMerge, collapse = '-'))
        # cat(file=stderr(), "categories - ", unique(dfAll[[input$grouping]]))
      }
      
      units <- unlist(lapply(mergedCats$merg, paste, collapse = '-')) # merged units in units
      units <- c(units, input$units[!(input$units %in% mergedCats$sep)]) # units which are not merged in units
      

    } else {
      
      units <- input$units
      
    }
    
    # filtering out units
    
    if (input$filterOut != 'none' & !is.null(input$filterOutUnit)){

      dfAll <- dfAll[!(dfAll[[input$filterOut]] %in% input$filterOutUnit), ]

    }
    
    if (input$influence_choice == 'whole'){
      
      # calculations for global level only
      
      if (input$level == 'global'){
        
          # data to YM plot
        
          dfMain1 <- dfAll %>%
            group_by(YEARMONTH) %>%
            summarise(statWith = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T),
                                        quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                      countAll = n()) %>%
            mutate(global = 'Global') %>%
            ungroup()
          
          # data to overall plot
          
          dfMarg1 <- dfAll %>%
            group_by() %>%
            summarise(statWith =  ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T),
                                         quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                      countAll = n()) %>%
            mutate(global = 'global') %>%
            ungroup()
          
          #cat(file=stderr(), "---", unique(dfMain1[1]))
          
          for (unit in units){
            
            # data to YM plot

            dfMain2 <- dfAll[dfAll[[input$grouping]] != unit, ]
            dfMarg2 <- dfAll[dfAll[[input$grouping]] != unit, ]
            
            dfMain2 <- dfMain2 %>%
              group_by(YEARMONTH) %>%
              summarise(statWithout = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T),
                                             quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                        countCatWithout = n()) %>%
              full_join(dfMain1, by = 'YEARMONTH') %>%
              mutate(statMov = (statWith - statWithout)/statWithout,
                     statDiff = statWith - statWithout,
                     catShare = (countAll - countCatWithout)/countAll) %>%
              ungroup()
            
            dfMain2[input$grouping] <- unit
            
            # data to overall plot
            
            
            dfMarg2 <- dfMarg2 %>%
              group_by() %>%
              summarise(statWithout = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T),
                                             quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                        countCatWithout = n()) %>%
              cbind(dfMarg1) %>%
              mutate(statMov = (statWith - statWithout)/statWithout,
                     statDiff = statWith - statWithout,
                     catShare = (countAll - countCatWithout)/countAll) %>%
              ungroup()
            
            dfMarg2[input$grouping] <- unit
            
            if (unit == units[1]){
              
              dfMain <- dfMain2
              dfMarg <- dfMarg2
              
            } else {
              
              dfMain <- dfMain %>%
                bind_rows(dfMain2)
              
              dfMarg <- dfMarg %>%
                bind_rows(dfMarg2)
              
            }

          }
          
      } else {
        
        if (input$level == 'selected'){
          
          # dorobit (percentualny pohyb len v ramci zvolenych grup)
          
        } else {
          
          # calculation for any other level except global (ex. ORG, GEO,...)
          
            # data to YM plot
          
          dfMain1 <- dfAll %>%
            group_by_(.dots = lapply(c(input$level, 'YEARMONTH'), as.symbol)) %>%
            summarise(statWith = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                        quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                      countAll = n()) %>%
            ungroup()
          
            # data to overall plot
          
          dfMarg1 <- dfAll %>%
            group_by_(.dots = lapply(input$level, as.symbol)) %>%
            summarise(statWith = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                        quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                      countAll = n()) %>%
            ungroup()
          
          for (unit in units){
            
            # data to YM plot
            
            dfMain2 <- dfAll[dfAll[[input$grouping]] != unit, ]
            dfMarg2 <- dfAll[dfAll[[input$grouping]] != unit, ]

            dfMain2 <- dfMain2 %>%
              group_by_(.dots = lapply(c(input$level, 'YEARMONTH'), as.symbol)) %>%
              summarise(statWithout = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                          quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                        countCatWithout = n()) %>%
              full_join(dfMain1, by = c(input$level, 'YEARMONTH')) %>%
              mutate(statMov = (statWith - statWithout)/statWithout,
                     statDiff = statWith - statWithout,
                     catShare = (countAll - countCatWithout)/countAll) %>%
              ungroup()
            
            dfMain2[input$grouping] <- unit
            
            # data to overall plot
            

            dfMarg2 <- dfMarg2 %>%
              group_by_(.dots = lapply(input$level, as.symbol)) %>%
              summarise(statWithout = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                             quantile(var_ub, probs = input$influenceQuantile, na.rm = T)),
                        countCatWithout = n()) %>%
              full_join(dfMarg1, by = input$level) %>%
              mutate(statMov = (statWith - statWithout)/statWithout,
                     statDiff = statWith - statWithout,
                     catShare = (countAll - countCatWithout)/countAll) %>%
              ungroup()
            
            dfMarg2[input$grouping] <- unit
            
            if (unit == units[1]){
              
              dfMain <- dfMain2
              dfMarg <- dfMarg2
              
            } else {
              
              dfMain <- dfMain %>%
                bind_rows(dfMain2)
              
              dfMarg <- dfMarg %>%
                bind_rows(dfMarg2)
              
            }
            
          }
          

        }
        
      }
      
      #dfMain <- dfMain[dfMain$countCatWithout != dfMain$countAll, ]
      
      # influenceDF$testdf <- dfMain
      influenceDF$mainPlot <- dfMain
      influenceDF$margPlot <- dfMarg
      
    } else {
      
      if (input$influence_choice == 'values'){
        
        if (input$level == 'global'){
          
          # main plot data
          
          dfMain1 <- dfAll %>%
            group_by_(.dots = lapply('YEARMONTH', as.symbol)) %>%
            summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                             quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
            ungroup()
          
          dfMain2 <- dfAll %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            group_by_(.dots = lapply(c(input$grouping, 'YEARMONTH'), as.symbol)) %>%
            summarise(countCat = n()) %>%
            ungroup()
          
          dfMain <- dfAll %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            left_join(dfMain1, by = 'YEARMONTH') %>%
            filter(var_ub < stat) %>%
            group_by_(.dots = lapply(c(input$grouping, 'YEARMONTH'), as.symbol)) %>%
            summarise(countCatUnder = n()) %>%
            full_join(dfMain2, by = c(input$grouping, 'YEARMONTH')) %>%
            ungroup()
          
          dfMain$countCatUnder <- ifelse(is.na(dfMain$countCatUnder), 0, dfMain$countCatUnder)
          
          dfMain <- dfMain %>%
            mutate(percUnder = countCatUnder/countCat) %>%
            mutate(percEqAbove = 1 - percUnder) %>%
            ungroup()
          
          dfMain$percUnder <- dfMain$percUnder * (-1)
          
          # overall plot data
          
          dfMarg1 <- dfAll %>%
            group_by() %>%
            summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                    quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
            ungroup()
          
          dfMarg2 <- dfAll %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
            summarise(countCat = n()) %>%
            ungroup()
          
          dfMarg <- dfAll %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            filter(var_ub < dfMarg1$stat) %>%
            group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
            summarise(countCatUnder = n()) %>%
            full_join(dfMarg2, by = input$grouping) %>%
            ungroup()
          
          dfMarg <- dfMarg %>%
            mutate(percUnder = countCatUnder/countCat) %>%
            mutate(percEqAbove = 1 - percUnder) %>%
            ungroup()
          
          dfMarg$percUnder <- dfMarg$percUnder * (-1)
          
          influenceDF$mainPlot <- dfMain
          influenceDF$margPlot <- dfMarg
          influenceDF$testdf <- dfMain
          
        } else {
          
          # main plot data
          
          dfMain1 <- dfAll %>%
            group_by_(.dots = lapply(c(input$level, 'YEARMONTH'), as.symbol)) %>%
            summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                    quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
            ungroup()
          
          dfMain2 <- dfAll %>%
            group_by_(.dots = lapply(c(input$grouping, input$level, 'YEARMONTH'), as.symbol)) %>%
            summarise(countCat = n()) %>%
            ungroup()
          
          dfMain <- dfAll %>%
            full_join(dfMain1, by = c(input$level, 'YEARMONTH')) %>%
            filter(var_ub < stat) %>%
            group_by_(.dots = lapply(c(input$grouping, input$level, 'YEARMONTH'), as.symbol)) %>%
            summarise(countCatUnder = n()) %>%
            full_join(dfMain2, by = c(input$grouping, input$level, 'YEARMONTH')) %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            mutate(percUnder = countCatUnder/countCat) %>%
            mutate(percEqAbove = 1 - percUnder) %>%
            ungroup()
          
          dfMain$percUnder <- dfMain$percUnder * (-1)
          
          # overall plot data
          
          dfMarg1 <- dfAll %>%
            group_by_(.dots = lapply(input$level, as.symbol)) %>%
            summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                    quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
            ungroup()
          
          dfMarg2 <- dfAll %>%
            group_by_(.dots = lapply(c(input$grouping, input$level), as.symbol)) %>%
            summarise(countCat = n()) %>%
            ungroup()
          
          dfMarg <- dfAll %>%
            full_join(dfMarg1, by = input$level) %>%
            filter(var_ub < stat) %>%
            group_by_(.dots = lapply(c(input$grouping, input$level), as.symbol)) %>%
            summarise(countCatUnder = n()) %>%
            full_join(dfMarg2, by = c(input$grouping, input$level)) %>%
            filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
            mutate(percUnder = countCatUnder/countCat) %>%
            mutate(percEqAbove = 1 - percUnder) %>%
            ungroup()
          
          dfMarg$percUnder <- dfMarg$percUnder * (-1)
          
          influenceDF$mainPlot <- dfMain
          influenceDF$margPlot <- dfMarg
          influenceDF$testdf <- dfMain
          
        }
        
      } else {
        
        if (input$influence_choice == 'share'){
          
          if (input$level == 'global'){
            
            # main plot data
            
            dfMain1 <- dfAll %>%
              group_by(YEARMONTH) %>%
              summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                      quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
              ungroup()
            
            dfMain2 <- dfAll %>%
              full_join(dfMain1, by = 'YEARMONTH') %>%
              filter(var_ub < stat) %>%
              group_by(YEARMONTH) %>%
              summarise(countAllUnder = n()) %>%
              ungroup()
            
            dfMain3 <- dfAll %>%
              group_by_(.dots = lapply(c(input$grouping, 'YEARMONTH'), as.symbol)) %>%
              summarise(countCatAll = n()) %>%
              ungroup()
            
            dfMain <- dfAll %>%
              full_join(dfMain1, by = 'YEARMONTH') %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(c(input$grouping, 'YEARMONTH'), as.symbol)) %>%
              summarise(countCatUnder = n()) %>%
              full_join(dfMain2, by = 'YEARMONTH') %>%
              full_join(dfMain3, by = c(input$grouping, 'YEARMONTH')) %>%
              filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
              mutate(percCat = countCatUnder/countAllUnder) %>%
              ungroup()
            
            # overall plot data
            
            dfMarg1 <- dfAll %>%
              group_by() %>%
              summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                      quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
              ungroup()
            
            dfMarg2 <- dfAll %>%
              cbind(dfMarg1) %>%
              filter(var_ub < stat) %>%
              group_by() %>%
              summarise(countAllUnder = n()) %>%
              ungroup()
            
            dfMarg3 <- dfAll %>%
              group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
              summarise(countCatAll = n()) %>%
              ungroup()
            
            dfMarg <- dfAll %>%
              cbind(dfMarg1) %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
              summarise(countCatUnder = n()) %>%
              cbind(dfMarg2) %>%
              full_join(dfMarg3, by = input$grouping) %>%
              filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
              mutate(percCat = countCatUnder/countAllUnder) %>%
              ungroup()
            
          } else {
            
            # main plot data
            
            dfMain1 <- dfAll %>%
              group_by_(.dots = lapply(c(input$level, 'YEARMONTH'), as.symbol)) %>%
              summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                      quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
              ungroup()
            
            dfMain2 <- dfAll %>%
              inner_join(dfMain1, by = c(input$level, 'YEARMONTH')) %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(c(input$level, 'YEARMONTH'), as.symbol)) %>%
              summarise(countAllUnder = n()) %>%
              ungroup()
            
            dfMain3 <- dfAll %>%
              group_by_(.dots = lapply(c(input$grouping, 'YEARMONTH'), as.symbol)) %>%
              summarise(countCatAll = n()) %>%
              ungroup()
            
            dfMain <- dfAll %>%
              full_join(dfMain1, by = c(input$level, 'YEARMONTH')) %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(c(input$grouping, input$level, 'YEARMONTH'), as.symbol)) %>%
              summarise(countCatUnder = n()) %>%
              full_join(dfMain2, by = c(input$level, 'YEARMONTH')) %>%
              full_join(dfMain3, by = c(input$grouping, 'YEARMONTH')) %>%
              filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
              mutate(percCat = countCatUnder/countAllUnder) %>%
              ungroup()
            
            # overall plot data
            
            dfMarg1 <- dfAll %>%
              group_by_(.dots = lapply(input$level, as.symbol)) %>%
              summarise(stat = ifelse(input$influenceOpts == 'mean', mean(var_ub, na.rm = T), 
                                      quantile(var_ub, probs = input$influenceQuantile, na.rm = T))) %>%
              ungroup()
            
            dfMarg2 <- dfAll %>%
              inner_join(dfMarg1, by = input$level) %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(input$level, as.symbol)) %>%
              summarise(countAllUnder = n()) %>%
              ungroup()
            
            dfMarg3 <- dfAll %>%
              group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
              summarise(countCatAll = n()) %>%
              ungroup()
            
            dfMarg <- dfAll %>%
              full_join(dfMarg1, by = input$level) %>%
              filter(var_ub < stat) %>%
              group_by_(.dots = lapply(c(input$grouping, input$level), as.symbol)) %>%
              summarise(countCatUnder = n()) %>%
              full_join(dfMarg2, by = input$level) %>%
              full_join(dfMarg3, by = input$grouping) %>%
              filter_(interp(~col %in% units, col = as.name(input$grouping))) %>%
              mutate(percCat = countCatUnder/countAllUnder) %>%
              ungroup()

            
          }
          
          influenceDF$mainPlot <- dfMain
          influenceDF$margPlot <- dfMarg
          influenceDF$testdf <- dfMain
          
        }
        
      }
      
    }
    
  })
  
  output$influence_plotMain <- renderPlot({
    
    req(influenceDF$mainPlot, input$units)
    
    df <- influenceDF$mainPlot
    
    # not plotting units selected to hide
    
    if (!is.null(input$influence_hide)){
      
      df <- df[!(df[[input$level]] %in% input$influence_hide), ]
      
    }
    
    # calculating sequence for lines between months

    if (input$influence_choice == 'values'){
      
      if (input$level == 'global'){
        
        seqLine <- df %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          ungroup()
        
        seqLine <- seqLine[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
      } else {
        
        se <- df %>%
          group_by_(.dots = lapply(c('YEARMONTH', input$grouping), as.symbol)) %>%
          summarise(c = n()) %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          mutate(a = l - YMc/2) %>%
          ungroup()
        
        seqLine <- se[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
        seqYM <- se[['a']]

      }
      
      ###
      
      if (input$level == 'global'){
        
        toPlot <- ggplot(aes_string(x = 'YEARMONTH'), data = df) +
          geom_bar(aes_string(y = 'percUnder', fill = input$grouping), stat = 'identity', position = 'dodge', alpha = 0.5) +
          geom_bar(aes_string(y = 'percEqAbove', fill = input$grouping), stat = 'identity', position = 'dodge', alpha = 0.5) +
          geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
          theme(panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'),
                axis.title.y = element_blank()) +
          scale_y_continuous(labels = scales::percent) +
          ggtitle('% of values under global statistic')
        
      } else {
        
        toPlot <- ggplot(data = NULL) +
          geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                              y = 'percUnder',
                              fill = ifelse(length(unique(df[[input$level]])) > 1, input$level, input$grouping)),
                   data = df,
                   stat = 'identity',
                   position = 'dodge',
                   alpha = 0.5) +
          geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                              y = 'percEqAbove',
                              fill = ifelse(length(unique(df[[input$level]])) > 1, input$level, input$grouping)),
                   data = df,
                   stat = 'identity',
                   position = 'dodge',
                   alpha = 0.5)
        
        if (length(unique(df[[input$level]])) > 1){
          
          toPlot <- toPlot +
            geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                                y = 'maxFill',
                                color = input$grouping),
                     fill = 'white',
                     alpha = 0.01,
                     stat = 'identity',
                     position = 'dodge',
                     data = df %>%
                       group_by() %>%
                       summarise(maxFill = max(percEqAbove, na.rm = T)) %>%
                       cbind(df)) +
            geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                                y = 'minFill',
                                color = input$grouping),
                     fill = 'white',
                     alpha = 0.01,
                     stat = 'identity',
                     position = 'dodge',
                     data = df %>%
                       group_by() %>%
                       summarise(minFill = min(percUnder, na.rm = T)) %>%
                       cbind(df))
          
        }
        
        toPlot <- toPlot +  
          geom_vline(xintercept = seqLine,
                     linetype = 2, size = 1) +
          geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
          theme(panel.background = element_rect(fill =NA),
                axis.text.x = element_blank(),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                panel.grid.major.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line = element_line(colour = '#bdbdbd'),
                axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          annotate('text',
                   x = seqYM,
                   y = min(df$percUnder, na.rm = T) - 0.04, label = unique(df[['YEARMONTH']])[order(unique(df[['YEARMONTH']]))]) +
          scale_y_continuous(labels = scales::percent) +
          ggtitle('% of values under statistic of selected level')
          
        
      }
    
      
    } else {
      
      if (input$influence_choice == 'share'){
        
        df$percCat <- ifelse(is.na(df$percCat), 0, df$percCat)
        
        if (input$level == 'global'){
          
          seqLine <- df %>%
            group_by(YEARMONTH) %>%
            summarise(YMc = n()) %>%
            mutate(l = cumsum(YMc) + 0.5) %>%
            ungroup()
          
          seqLine <- seqLine[['l']]
          seqLine <- seqLine[1:(length(seqLine) - 1)]
          
        } else {
          
          se <- df %>%
            group_by_(.dots = lapply(c('YEARMONTH', input$grouping), as.symbol)) %>%
            summarise(c = n()) %>%
            group_by(YEARMONTH) %>%
            summarise(YMc = n()) %>%
            mutate(l = cumsum(YMc) + 0.5) %>%
            mutate(a = l - YMc/2) %>%
            ungroup()
          
          seqLine <- se[['l']]
          seqLine <- seqLine[1:(length(seqLine) - 1)]
          
          seqYM <- se[['a']]
          
        }
        
        if (input$level == 'global'){
        
          toPlot <- ggplot(aes_string(x = 'YEARMONTH', fill = input$grouping), data = df) +
            geom_bar(aes(y = percCat), stat = 'identity', position = 'dodge', alpha = 0.5) +
            theme(panel.background = element_rect(fill =NA),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  axis.line = element_line(colour = '#BDBDBD'),
                  axis.title.y = element_blank()) +
            scale_y_continuous(labels = scales::percent) +
            ggtitle('% share from values under global statistic')
          
        } else {
          
          toPlot <- ggplot(data = NULL) +
            geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                                y = 'percCat',
                                fill = ifelse(length(unique(df[[input$level]])) > 1, input$level, input$grouping)),
                     data = df,
                     stat = 'identity',
                     position = 'dodge',
                     alpha = 0.5)
          
          if (length(unique(df[[input$level]])) > 1){
            
            toPlot <- toPlot +
              geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]),
                                  y = 'maxFill',
                                  color = input$grouping),
                       fill = 'white',
                       alpha = 0.01,
                       stat = 'identity',
                       position = 'dodge',
                       data = df %>%
                         group_by() %>%
                         summarise(maxFill = max(percCat, na.rm = T)) %>%
                         cbind(df)) 
            
          }
        
          toPlot <- toPlot +  
            geom_vline(xintercept = seqLine,
                       linetype = 2, size = 1) +
            theme(panel.background = element_rect(fill =NA),
                  axis.text.x = element_blank(),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  panel.grid.major.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line = element_line(colour = '#bdbdbd'),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            annotate('text',
                     x = seqYM,
                     y = max(df$percCat) * (-0.05), label = unique(df[['YEARMONTH']])[order(unique(df[['YEARMONTH']]))]) +
            scale_y_continuous(labels = scales::percent) +
            ggtitle('% share from values under statistic of selected level')
          
        }
        
      } else {
        
        if (input$influence_choice == 'whole'){
          
          # df <- df[!is.na(df$statMov) & !is.nan(df$statMov) & !is.infinite(df$statMov), ]# because chart is not generated due to dividing by zero result NaN, Inf is misleading on chart

          df$statMov <- ifelse(is.na(df$statMov) | is.nan(df$statMov) | is.infinite(df$statMov), 0, df$statMov)
          df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
          
          df <- df[df$catShare != 0, ]
          
          #influenceDF$testdf2 <- df
          influenceDF$testdf <- df
          
          # calculating sequences for vertical lines
          
          if (input$level == 'global'){
            
            se <- df %>%
              group_by(YEARMONTH) %>%
              summarise(YMc = n()) %>%
              mutate(l = cumsum(YMc) + 0.5) %>%
              mutate(a = l - YMc/2) %>%
              ungroup()
            
            seqLine <- se[['l']]
            seqLine <- seqLine[1:(length(seqLine) - 1)]
            
            seqYM <- se[['a']]
            
          } else {
            
            se <- df %>%
              group_by_(.dots = lapply(c('YEARMONTH', input$grouping), as.symbol)) %>%
              summarise(c = n()) %>%
              group_by(YEARMONTH) %>%
              summarise(YMc = n()) %>%
              mutate(l = cumsum(YMc) + 0.5) %>%
              mutate(a = l - YMc/2) %>%
              ungroup()
            
            seqLine <- se[['l']]
            seqLine <- seqLine[1:(length(seqLine) - 1)]
            
            seqYM <- se[['a']]
            
          }
          
          #cat(file=stderr(), "---", seqLine) 

          toPlot <- 
            ggplot(data = NULL) +
              
              geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                                  y = 'statMov', 
                                  fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[df$statMov > 0, ][[input$level]])) == 1), input$grouping, input$level)), 
                       stat = 'identity', 
                       position = 'dodge', 
                       alpha = 0.5,
                       data = df) +
              theme(panel.background = element_rect(fill =NA),
                    axis.text.x = element_blank(),
                    panel.grid.major = element_line(colour = '#e5e5e5'),
                    panel.grid.major.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.line = element_line(colour = '#bdbdbd'),
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank()) +
              geom_vline(xintercept = seqLine, linetype = 2, size = 1) +
              geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
              ggtitle('% difference') +
            scale_y_continuous(labels = scales::percent)
          
          if (input$level != 'global' & length(unique(df[df$statMov > 0, ][[input$level]])) > 1){
            
            if (max(df$statMov) > 0){
            
              toPlot <- toPlot +
                geom_bar(stat = 'identity',
                         position = 'dodge',
                         aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                                    y = 'maxFill', 
                                    colour = input$grouping), 
                         data = df %>%
                           group_by() %>%
                           summarise(maxFill = max(statMov, na.rm = T)) %>%
                           cbind(df),
                         alpha = 0.01,
                         fill = 'white')
            
            }
            
            if (min(df$statMov) < 0){
              
              toPlot <- toPlot +
                geom_bar(stat = 'identity',
                         position = 'dodge',
                         aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                                    y = 'minFill', 
                                    colour = input$grouping), 
                         data = df %>%
                           group_by() %>%
                           summarise(minFill = min(statMov, na.rm = T)) %>%
                           cbind(df),
                         alpha = 0.01,
                         fill = 'white')
              
            }
            
          }
          
        }
        
      }
      
    }

    toPlot
    
  })
  
  output$influence_plotDiff <- renderPlot({
    
    req(influenceDF$mainPlot, input$units)
    
    df <- influenceDF$mainPlot
    
    if (!is.null(input$influence_hide)){
      
      df <- df[!(df[[input$level]] %in% input$influence_hide), ]
      
    }
    
    if (input$influence_choice == 'whole'){
      
      # df$statMov <- ifelse(is.na(df$statMov) | is.nan(df$statMov) | is.infinite(df$statMov), 0, df$statMov)
      # df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
      
      #df <- df[!is.na(df$statMov) & !is.nan(df$statMov) & !is.infinite(df$statMov), ]
      df$statDiff <- ifelse(is.na(df$statDiff) | is.nan(df$statDiff) | is.infinite(df$statDiff), 0, df$statDiff)
      df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
      df <- df[df$catShare != 0, ]
      
      if (input$level == 'global'){
        
        seqLine <- df %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          ungroup()
        
        seqLine <- seqLine[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
      } else {
        
        seqLine <- df %>%
          group_by_(.dots = lapply(c('YEARMONTH', input$grouping), as.symbol)) %>%
          summarise(c = n()) %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          ungroup()
        
        seqLine <- seqLine[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
      }
      
      toPlot <- 
        ggplot(data = NULL) +
        
        geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                            y = 'statDiff', 
                            fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[df$statDiff > 0, ][[input$level]])) == 1), input$grouping, input$level)), 
                 stat = 'identity', 
                 position = 'dodge', 
                 alpha = 0.5,
                 data = df) +
        theme(panel.background = element_rect(fill =NA),
              axis.text.x = element_blank(),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              panel.grid.major.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(colour = '#bdbdbd'),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        geom_vline(xintercept = seqLine,
                   linetype = 2, size = 1) +
        geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
        ggtitle('Difference')
      
      if (input$level != 'global' & length(unique(df[df$statDiff > 0, ][[input$level]])) > 1){
        
        if (max(df$statDiff) > 0){
        
          toPlot <- toPlot +
            geom_bar(stat = 'identity',
                     position = 'dodge',
                     aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                                y = 'maxDiff', 
                                colour = input$grouping), 
                     data = df %>%
                       group_by() %>%
                       summarise(maxDiff = max(statDiff, na.rm = T)) %>%
                       cbind(df),
                     alpha = 0.01,
                     fill = 'white')
        
        }
        
        if (min(df$statDiff) < 0){
          
          toPlot <- toPlot +
            geom_bar(stat = 'identity',
                     position = 'dodge',
                     aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                                y = 'minDiff', 
                                colour = input$grouping), 
                     data = df %>%
                       group_by() %>%
                       summarise(minDiff = min(statDiff, na.rm = T)) %>%
                       cbind(df),
                     alpha = 0.01,
                     fill = 'white')
          
        }
        
      }
      
      toPlot
      
    }
    
    
    
  })
  
  output$influence_plotShare <- renderPlot({
    
    req(influenceDF$mainPlot, input$units)
    
    df <- influenceDF$mainPlot
    
    if (!is.null(input$influence_hide)){
      
      df <- df[!(df[[input$level]] %in% input$influence_hide), ]
      
    }
    
    if (input$influence_choice == 'whole'){
      
      #df$statDiff <- ifelse(is.na(df$statDiff) | is.nan(df$statDiff) | is.infinite(df$statDiff), 0, df$statDiff)
      df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
      df <- df[df$catShare != 0, ]
      
      if (input$level == 'global'){
        
        se <- df %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          mutate(a = l - YMc/2) %>%
          ungroup()
        
        seqLine <- se[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
        seqYM <- se[['a']]
        
      } else {
        
        se <- df %>%
          group_by_(.dots = lapply(c('YEARMONTH', input$grouping), as.symbol)) %>%
          summarise(c = n()) %>%
          group_by(YEARMONTH) %>%
          summarise(YMc = n()) %>%
          mutate(l = cumsum(YMc) + 0.5) %>%
          mutate(a = l - YMc/2) %>%
          ungroup()
        
        seqLine <- se[['l']]
        seqLine <- seqLine[1:(length(seqLine) - 1)]
        
        seqYM <- se[['a']]
        
      }
      
      toPlot <- 
        ggplot(data = NULL) +
       
        geom_bar(aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                            y = 'catShare', fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[df$catShare > 0, ][[input$level]])) == 1), input$grouping, input$level)), 
                 stat = 'identity', 
                 position = 'dodge', 
                 alpha = 0.5,
                 data = df) +
        annotate('text',
                 x = seqYM ,
                 y = max(df$catShare) * (-0.05), label = unique(df[['YEARMONTH']])) +
        theme(panel.background = element_rect(fill =NA),
              axis.text.x = element_blank(),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              panel.grid.major.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(colour = '#bdbdbd'),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        geom_vline(xintercept = seqLine,
                   linetype = 2, size = 1) +
        ggtitle('Share') +
        scale_y_continuous(labels = scales::percent)
      
      if (input$level != 'global' & length(unique(df[df$catShare > 0, ][[input$level]])) > 1){
        
       toPlot <- toPlot +
         
         geom_bar(stat = 'identity',
                  position = 'dodge',
                  aes_string(x = interaction(df[[input$grouping]], df[['YEARMONTH']]), 
                             y = 'maxShare', 
                             colour = input$grouping), 
                  data = df %>%
                    group_by() %>%
                    summarise(maxShare = max(catShare, na.rm = T)) %>%
                    cbind(df),
                  alpha = 0.01,
                  fill = 'white') 
      }
      
      toPlot
      
    }
    
  })
  
  output$influence_plotOverall <- renderPlot({
    
    req(influenceDF$margPlot, input$units)
    
    df <- influenceDF$margPlot
    
    if (input$influence_choice == 'values'){
      
      if (!is.null(input$influence_hide)){
        
        df <- df[!(df[[input$level]] %in% input$influence_hide), ]
        
      }
      
      if (input$level == 'global'){
        
        toPlot <- ggplot(aes_string(x = input$grouping, fill = input$grouping), data = df) +
          geom_bar(aes(y = percUnder), stat = 'identity', position = 'dodge', alpha = 0.5) +
          geom_bar(aes(y = percEqAbove), stat = 'identity', position = 'dodge', alpha = 0.5) +
          geom_hline(yintercept = 0, linetype = 2, size = 2, color = 'red') +
          theme(legend.position = 'none',
                panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'),
                axis.title.y = element_blank()) +
          scale_y_continuous(labels = scales::percent) +
          ggtitle('% of values under global statistic')
        
      } else {
        
        toPlot <- ggplot(data = NULL) +
          geom_bar(aes_string(x = input$grouping,
                              y = 'percUnder',
                              fill = ifelse(length(unique(df[[input$level]])) > 1, input$level, input$grouping)),
                   data = df,
                   stat = 'identity',
                   position = 'dodge',
                   alpha = 0.5) +
          geom_bar(aes_string(x = input$grouping,
                              y = 'percEqAbove',
                              fill = ifelse(length(unique(df[[input$level]])) > 1, input$level, input$grouping)),
                   data = df,
                   stat = 'identity',
                   position = 'dodge',
                   alpha = 0.5)
        
        if (length(unique(df[[input$level]])) > 1){
          
          toPlot <- toPlot +
            geom_bar(aes_string(x = input$grouping,
                                y = 'maxFill',
                                color = input$grouping),
                     fill = 'white',
                     alpha = 0.01,
                     stat = 'identity',
                     position = 'dodge',
                     data = df %>%
                       group_by() %>%
                       summarise(maxFill = max(percEqAbove, na.rm = T)) %>%
                       cbind(df)) +
            geom_bar(aes_string(x = input$grouping,
                                y = 'minFill',
                                color = input$grouping),
                     fill = 'white',
                     alpha = 0.01,
                     stat = 'identity',
                     position = 'dodge',
                     data = df %>%
                       group_by() %>%
                       summarise(minFill = min(percUnder, na.rm = T)) %>%
                       cbind(df))
          
        }
        
        toPlot <- toPlot +
          geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
          theme(panel.background = element_rect(fill =NA),
                axis.text.x = element_blank(),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                panel.grid.major.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line = element_line(colour = '#bdbdbd'),
                axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          ggtitle('% of values under statistic of selected level') +
          scale_y_continuous(labels = scales::percent)
        
      }
    
      
    } else {
      
      if (input$influence_choice == 'share'){
        
        if (!is.null(input$influence_hide)){
          
          df <- df[!(df[[input$level]] %in% input$influence_hide), ]
          
        }
        
        if (input$level == 'global'){
        
          toPlot <- ggplot(aes_string(x = input$grouping, fill = input$grouping), data = df) +
            geom_bar(aes(y = percCat), stat = 'identity', position = 'dodge', alpha = 0.5) +
            theme(legend.position = 'none',
                  panel.background = element_rect(fill =NA),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  axis.line = element_line(colour = '#BDBDBD'),
                  axis.title.y = element_blank()) +
            scale_y_continuous(labels = scales::percent)
          
        } else {
          
          toPlot <- 
            ggplot(data = NULL) +
            
            geom_bar(aes_string(x = input$grouping, 
                                y = 'percCat', 
                                fill = ifelse(length(unique(df[[input$level]])) == 1, input$grouping, input$level)), 
                     stat = 'identity', 
                     position = 'dodge', 
                     alpha = 0.5,
                     data = df) +
            theme(panel.background = element_rect(fill =NA),
                  axis.text.x = element_blank(),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  panel.grid.major.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line = element_line(colour = '#bdbdbd'),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            ggtitle('% share from values under statistic of selected level') +
            scale_y_continuous(labels = scales::percent)
          
          if (length(unique(df[df$percCat > 0, ][[input$level]])) > 1){
            
            toPlot <- toPlot +
              
              geom_bar(stat = 'identity',
                       position = 'dodge',
                       aes_string(x = input$grouping, 
                                  y = 'maxShare', 
                                  colour = input$grouping), 
                       data = df %>%
                         group_by() %>%
                         summarise(maxShare = max(percCat, na.rm = T)) %>%
                         cbind(df),
                       alpha = 0.01,
                       fill = 'white') 
          }
          
        }
        
      } else {
        
        if (input$influence_choice == 'whole'){
          
          if (!is.null(input$influence_hide)){
            
            df <- df[!(df[[input$level]] %in% input$influence_hide), ]
            
          }
          
          # df$statMov <- ifelse(is.na(df$statMov) | is.nan(df$statMov) | is.infinite(df$statMov), 0, df$statMov)
          # df <- df[df$catShare != 0, ]
          
          df$statMov <- ifelse(is.na(df$statMov) | is.nan(df$statMov) | is.infinite(df$statMov), 0, df$statMov)
          df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
          
          df <- df[df$catShare != 0, ]
          
          toPlot <- 
            ggplot(data = NULL) +
            
            geom_bar(aes_string(x = input$grouping, 
                                y = 'statMov',
                                fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[[input$level]])) == 1), input$grouping, input$level)), 
                     stat = 'identity', 
                     position = 'dodge', 
                     alpha = 0.5,
                     data = df) +
            theme(panel.background = element_rect(fill =NA),
                  axis.text.x = element_blank(),
                  panel.grid.major = element_line(colour = '#e5e5e5'),
                  panel.grid.major.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line = element_line(colour = '#bdbdbd'),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()) +
            geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
            ggtitle('% difference') +
            scale_y_continuous(labels = scales::percent)
          
          if (input$level != 'global' & length(unique(df[[input$level]])) > 1){
            
            if (max(df$statMov) >= 0){
              
              toPlot <- toPlot +
                geom_bar(stat = 'identity',
                         position = 'dodge',
                         aes_string(x = input$grouping, 
                                    y = 'maxFill', 
                                    colour = input$grouping), 
                         data = df %>%
                           group_by() %>%
                           summarise(maxFill = max(statMov, na.rm = T)) %>%
                           cbind(df),
                         alpha = 0.01,
                         fill = 'white')
              
            }
            
            if (min(df$statMov) <= 0){
              
              toPlot <- toPlot +
                geom_bar(stat = 'identity',
                         position = 'dodge',
                         aes_string(x = input$grouping, 
                                    y = 'minFill', 
                                    colour = input$grouping), 
                         data = df %>%
                           group_by() %>%
                           summarise(minFill = min(statMov, na.rm = T)) %>%
                           cbind(df),
                         alpha = 0.01,
                         fill = 'white')
              
            }
            
          }
          
        }
        
      }
      
    }
    
    toPlot
    
  })
  
  output$influence_plotDiffOverall <- renderPlot({
    
    
    req(influenceDF$margPlot, input$units)
    
    df <- influenceDF$margPlot
    
    if (!is.null(input$influence_hide)){
      
      df <- df[!(df[[input$level]] %in% input$influence_hide), ]
      
    }
    
    if (input$influence_choice == 'whole'){
      
      df$statDiff <- ifelse(is.na(df$statDiff) | is.nan(df$statDiff) | is.infinite(df$statDiff), 0, df$statDiff)
      df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
      df <- df[df$catShare != 0, ]
      
      toPlot <- 
        ggplot(data = NULL) +
        
        geom_bar(aes_string(x = input$grouping, 
                            y = 'statDiff', 
                            fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[[input$level]])) == 1), input$grouping, input$level)), 
                 stat = 'identity', 
                 position = 'dodge', 
                 alpha = 0.5,
                 data = df) +
        theme(panel.background = element_rect(fill =NA),
              axis.text.x = element_blank(),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              panel.grid.major.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(colour = '#bdbdbd'),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        geom_hline(yintercept = 0, linetype = 2, size = 1, color = 'red') +
        ggtitle('Difference')
      
      if (input$level != 'global' & length(unique(df[[input$level]])) > 1){
        
        if (max(df$statDiff) > 0){
          
          toPlot <- toPlot +
            geom_bar(stat = 'identity',
                     position = 'dodge',
                     aes_string(x = input$grouping, 
                                y = 'maxDiff', 
                                colour = input$grouping), 
                     data = df %>%
                       group_by() %>%
                       summarise(maxDiff = max(statDiff, na.rm = T)) %>%
                       cbind(df),
                     alpha = 0.01,
                     fill = 'white')
          
        }
        
        if (min(df$statDiff) < 0){
          
          toPlot <- toPlot +
            geom_bar(stat = 'identity',
                     position = 'dodge',
                     aes_string(x = input$grouping, 
                                y = 'minDiff', 
                                colour = input$grouping), 
                     data = df %>%
                       group_by() %>%
                       summarise(minDiff = min(statDiff, na.rm = T)) %>%
                       cbind(df),
                     alpha = 0.01,
                     fill = 'white')
          
        }
        
      }
      
      toPlot
      
    }
    
  })
  
  output$influence_plotShareOverall <- renderPlot({
    
    req(influenceDF$margPlot, input$units)
    
    df <- influenceDF$margPlot
    
    if (!is.null(input$influence_hide)){
      
      df <- df[!(df[[input$level]] %in% input$influence_hide), ]
      
    }
    
    if (input$influence_choice == 'whole'){
      
      df$catShare <- ifelse(is.na(df$countCatWithout), 1, df$catShare)
      df <- df[df$catShare != 0, ]
      
      toPlot <- 
        ggplot(data = NULL) +
        
        geom_bar(aes_string(x = input$grouping, 
                            y = 'catShare', 
                            fill = ifelse(input$level == 'global' | (input$level != 'global' & length(unique(df[df$catShare > 0, ][[input$level]])) == 1), input$grouping, input$level)), 
                 stat = 'identity', 
                 position = 'dodge', 
                 alpha = 0.5,
                 data = df) +
        theme(panel.background = element_rect(fill =NA),
              axis.text.x = element_blank(),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              panel.grid.major.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(colour = '#bdbdbd'),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        ggtitle('Share') +
        scale_y_continuous(labels = scales::percent)
      
      if (input$level != 'global' & length(unique(df[df$catShare > 0, ][[input$level]])) > 1){
        
        toPlot <- toPlot +
          
          geom_bar(stat = 'identity',
                   position = 'dodge',
                   aes_string(x = input$grouping, 
                              y = 'maxShare', 
                              colour = input$grouping), 
                   data = df %>%
                     group_by() %>%
                     summarise(maxShare = max(catShare, na.rm = T)) %>%
                     cbind(df),
                   alpha = 0.01,
                   fill = 'white') 
      }
      
      toPlot
      
    }
    
  })
  
  output$influence_table <- DT:: renderDataTable({
    
    if (input$influence_table_choice == 'month'){
      
      influenceDF$mainPlot
      
    } else {
      
      influenceDF$margPlot
      
    }
    
  })
  
  output$Utilization_marginal1 <- renderPlot({
    
    df <- dfToPlot$df
    
    if (!is.null(dfToPlot$df) & !is.null(input$units)){
      
      #cat(file=stderr(), "-------------PLOTDF---------", unique(df$GEO_NAME))
      
      plot_util_marg1 <- ggplot(aes(x = var_ub), data = df) +
        geom_histogram(fill = '#F79420', color = 'black', bins = input$bins, alpha = 0.5) +
        geom_vline(xintercept = mean(df$var_ub)) +
        geom_vline(xintercept = as.numeric(mean(df$var_ub, na.rm = T)),
                   color = 'red') +
        geom_vline(xintercept = as.numeric(median(df$var_ub, na.rm = T)),
                   color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$var_ub,
                                                    probs = 0.25, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$var_ub,
                                                    probs = 0.75, na.rm = T)),
                   linetype = 2, color = 'blue') +
        geom_vline(xintercept = as.numeric(quantile(df$var_ub,
                                                    probs = 0.1, na.rm = T)),
                   linetype = 3) +
        geom_vline(xintercept = as.numeric(quantile(df$var_ub,
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
    dfGlob <- pass_df()
    g <- gr$x
    
    if (input$mainPlotVis == 'Boxplots'){
      
      plot_util_YM <- ggplot(aes_string(y = 'var_ub', fill = g), data = df) + 
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
      
      if (input$globalSummaries == T){
        
        plot_util_YM <- plot_util_YM +
          geom_line(aes(x = factor(YEARMONTH), group = g), data = dfGlob,
                    fun.y = mean, stat = 'summary', color = 'red', size = 1, linetype = 2) +
          geom_line(aes(x = factor(YEARMONTH), group = g), data = dfGlob,
                    fun.y = median, stat = 'summary', color = 'blue', size = 1, linetype = 2)
        
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
            
            if (input$mainPlotVis == 'Point chart - user count'){
              
              if (input$pointChartOpts == 'mean'){
                
                plot_util_YM <- ggplot(data = NULL) +
                  geom_point(aes_string(x = 'YEARMONTH', y = 'm_var_ub', size = 'count', color = g),
                             alpha = 0.4,
                             position = position_dodge(width = 0.75),
                             data = df %>% 
                               group_by_(.dots = lapply(c('YEARMONTH', g), as.symbol)) %>%
                               summarise(count = n(), m_var_ub = mean(var_ub)) %>%
                               ungroup()) +
                  scale_size_continuous(range = c(1,20)) +
                  theme(panel.background = element_rect(fill =NA),
                        panel.grid.major = element_line(colour = '#e5e5e5'),
                        axis.line = element_line(colour = '#BDBDBD'))
                
              }
              
              if (input$pointChartOpts == 'quant'){
                
                plot_util_YM <- ggplot(data = NULL) +
                  geom_point(aes_string(x = 'YEARMONTH', y = 'm_var_ub', size = 'count', color = g),
                             alpha = 0.4,
                             position = position_dodge(width = 0.75),
                             data = df %>% 
                               group_by_(.dots = lapply(c('YEARMONTH', g), as.symbol)) %>%
                               summarise(count = n(), m_var_ub = quantile(var_ub, probs = input$pointChartQuantile, na.rm = T)) %>%
                               ungroup())+
                  scale_size_continuous(range = c(1,20)) +
                  theme(panel.background = element_rect(fill =NA),
                        panel.grid.major = element_line(colour = '#e5e5e5'),
                        axis.line = element_line(colour = '#BDBDBD'))
                
              }
              
              if (input$monthlySummaries == T){
                
                plot_util_YM <- plot_util_YM +
                  geom_line(aes(x = YEARMONTH, y = var_ub, group = g),
                            data = df,
                            fun.y = mean,
                            stat = 'summary',
                            color = 'red',
                            alpha = 0.4,
                            size = 2) +
                  geom_line(aes(x = YEARMONTH, y = var_ub, group = g),
                            data = df,
                            fun.y = median,
                            stat = 'summary',
                            color = 'blue',
                            alpha = 0.4,
                            size = 2)
                
              }
              
              if (input$globalSummaries == T){
                
                plot_util_YM <- plot_util_YM +
                  geom_line(aes(x = factor(YEARMONTH), y = var_ub, group = g), data = dfGlob,
                            fun.y = mean, stat = 'summary', color = 'red', size = 1, linetype = 2) +
                  geom_line(aes(x = factor(YEARMONTH), y = var_ub, group = g), data = dfGlob,
                            fun.y = median, stat = 'summary', color = 'blue', size = 1, linetype = 2)
                
              }
              
              
            }
            
          }
          
        }
        
      }
      
    }
    
    
    
    
    
    if (!is.null(range) & (input$mainPlotVis == 'Boxplots' | input$mainPlotVis == 'Point chart - user count')){
      
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
        
        plot_util_marg2 <- ggplot(aes_string(y = 'var_ub', x = g, fill = g), data = df) +
          geom_boxplot(alpha = 0.5) +
          geom_point(fun.y = mean, stat = 'summary', shape = 1) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.1),
                     stat = 'summary', shape = 4) +
          geom_point(fun.y = quantile, fun.args=list(probs=0.9),
                     stat = 'summary', shape = 4) +
          geom_hline(yintercept = as.numeric(mean(df$var_ub, na.rm = T)),
                     color = 'red') +
          geom_hline(yintercept = as.numeric(median(df$var_ub, na.rm = T)),
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
          
          plot_util_marg2 <- ggplot(aes_string(x = 'var_ub', fill = g), data = df) +
            geom_histogram(alpha = 0.4, position = 'identity', bins = input$bins) + 
            geom_vline(xintercept = as.numeric(mean(df$var_ub, na.rm = T)),
                       color = 'red') +
            geom_vline(xintercept = as.numeric(median(df$var_ub, na.rm = T)),
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
            
            plot_util_marg2 <- ggplot(aes_string(x = 'var_ub', fill = g), data = df) +
              geom_histogram(bins = input$bins, alpha = 0.5) +
              geom_vline(xintercept = as.numeric(mean(df$var_ub, na.rm = T)),
                         color = 'red') +
              geom_vline(xintercept = as.numeric(median(df$var_ub, na.rm = T)),
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
              
              plot_util_marg2 <- ggplot(aes_string(x = 'var_ub', fill = g), data = df) +
                geom_histogram(position = 'fill', bins = input$bins, alpha = 0.5) +
                ylab('%') +
                geom_vline(xintercept = as.numeric(mean(df$var_ub, na.rm = T)),
                           color = 'red') +
                geom_vline(xintercept = as.numeric(median(df$var_ub, na.rm = T)),
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
        
        if (input$input_chartType == 'box'){
          
          plotlyOutput('utilization_inputs_box',
                       height = '1200px')
          
        } else {
          
          if (input$input_chartType == 'densityFacet'){
            
            plotOutput('utilization_inputs_density',
                       height = '1200px')
            
          } else {
            
            if (input$input_chartType == 'densityAll'){
              
              plotOutput('utilization_inputs_densityAll',
                         height = '750px')
              
            }
            
          }
          
        }
        
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
        
        plot_util_rel <- ggplot(aes_string(x = input$util_inputs, y = 'var_ub'), data = df) +
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
          
          plot_util_rel <- ggplot(aes_string(x = input$util_inputs, y = 'var_ub', color = input$color_var), data = df) +
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
  
  output$utilization_inputs_density <- renderPlot({
    
    req(dfToPlot$df, input$units, input$util_inputs)
    
    df <- dfToPlot$df
    
    
    
    if (input$density_plotType == 'poly'){
      
      plot_dens <- ggplot(aes_string(x = input$util_inputs, y = 'var_ub'), data = df) +
        stat_density2d(aes(fill = ..level..), geom = 'polygon') +
        scale_fill_gradientn(colours = colorRampPalette(c('blue', 'yellow',"orange", "red", 'darkred'))(100)) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold'),
              legend.position = 'none')
      
    } else {
      
      plot_dens <- ggplot(aes_string(x = input$util_inputs, y = 'var_ub'), data = df) +
        stat_density2d(aes(fill = ..density.., alpha=ifelse(..density.. < 1e-5, 0, 1)), geom = 'tile', contour = F) +
        scale_fill_gradientn(colours = colorRampPalette(c('white','blue', 'yellow',"orange", "red", 'darkred'))(100)) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold'),
              legend.position = 'none')
      
      
    }
    
    if (input$include_summary == T){
      
      plot_dens <- plot_dens +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)), margins = T)
      
    } else {
      
      plot_dens <- plot_dens +
        facet_grid(as.formula(paste('YEARMONTH', ' ~ ', input$grouping)))
      
    }
    
    plot_dens
    
  })
  
  output$utilization_inputs_densityAll <- renderPlot({
    
    req(dfToPlot$df, input$units, input$util_inputs_dens)
    
    df <- dfToPlot$df
    
    if (input$density_plotType == 'poly'){
      
      # polygon
      
      plot_densAll <- ggplot(aes_string(x = input$util_inputs_dens, y = 'var_ub'), data = df) +
        stat_density2d(aes(fill = ..level..), geom = 'polygon') +
        scale_fill_gradientn(colours = colorRampPalette(c('blue', 'yellow',"orange", "red", 'darkred'))(100)) +
        theme(panel.background = element_rect(fill =NA),
              panel.grid.major = element_line(colour = '#e5e5e5'),
              axis.line = element_line(colour = '#BDBDBD'),
              strip.background = element_rect(fill = '#e5e5ff'),
              strip.text = element_text(face = 'bold'),
              legend.position = 'none')
      
    } else {
      
      if (input$density_plotType == 'heatmap'){
        
        # heatmap
        
        plot_densAll <- ggplot(aes_string(x = input$util_inputs_dens, y = 'var_ub'), data = df) +
          stat_density2d(aes(fill = ..density.., alpha=ifelse(..density.. < 1e-5, 0, 1)), geom = 'tile', contour = F) +
          scale_fill_gradientn(colours = colorRampPalette(c('white','blue', 'yellow',"orange", "red", 'darkred'))(100)) +
          theme(panel.background = element_rect(fill =NA),
                panel.grid.major = element_line(colour = '#e5e5e5'),
                axis.line = element_line(colour = '#BDBDBD'),
                strip.background = element_rect(fill = '#e5e5ff'),
                strip.text = element_text(face = 'bold'),
                legend.position = 'none')
        
      }
      
    }
    
    plot_densAll
    
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
    
    bin_plot <- ggplot(aes_string(x = 'xBinned', y = 'var_ub', fill = input$grouping), data = df) +
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
    
    #input$render_three_button
    
    #c(input$three_x_var, input$)
    
    df <- dfToPlot$df
    
   # isolate(
      
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
      
      
    #)
    
    #isolate(
      
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
      
    #)
    
    chartToPlot <- scatterplot3js(x = df[[input$three_x_var]], 
                                          y = df[[input$three_y_var]], 
                                          z = df$var_ub, 
                                          color = three_color, 
                                          size = three_size,
                                          renderer = 'canvas')
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
        
        plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'var_ub'),
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
          
          plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'var_ub', color = input$color_var_select),
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
            
            plot_selected <- ggplot(aes_string(x = input$util_inputs_selected, y = 'var_ub', color = input$color_var_select),
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
    
    tab <- rpivotTable(df_util, rows = 'GEO_NAME', cols = 'YEARMONTH', aggregatorName = 'Average', vals = 'var_ub',
                       rendererName = 'Table')
    
    tab
    
  })
  
  plots_clickDF <- reactiveValues(main_dTable = NULL, clicked_unit = NULL, clicked_YM = NULL)
  
  observeEvent(input$main_click, {
    
    df <- dfToPlot$df
    
    YM_levels <- levels(as.factor(df$YEARMONTH))
    YM_clicked <- YM_levels[round(input$main_click$x)]
    
    plots_clickDF$clicked_YM <- YM_clicked
    
    unit_levelsYM <- levels(as.factor(df[df$YEARMONTH == YM_clicked, input$grouping][[input$grouping]]))
    
    YM_lower_bound <- round(input$main_click$x) - (0.75/2)
    YM_upper_bound <- round(input$main_click$x) + (0.75/2)
    YM_intervals <- seq(from = YM_lower_bound, to = YM_upper_bound, by = 0.75/length(unit_levelsYM))
    
    
    clicked_YM_interval <- cut(input$main_click$x, YM_intervals, include.lowest = T)
    unit_YM_position <- match(as.character(clicked_YM_interval), as.character(levels(clicked_YM_interval)))
    
    clicked_unit <- unit_levelsYM[unit_YM_position]
    
    plots_clickDF$clicked_unit <- clicked_unit
    
    # for main datatable
    
    all_unitsDF <- pass_df()
    all_unitsDF <- all_unitsDF[all_unitsDF$YEARMONTH == YM_clicked,] # filter out not clicked months
    
    helpDF <- all_unitsDF %>%
      summarise(var_tb_sumAll = sum(var_tb),
                var_ti_sumAll = sum(var_ti),
                countAll = n())
    
    all_unitsDF <- all_unitsDF %>%
      group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
      summarise(median = round(median(var_ub), 2),
                mean = round(mean(var_ub), 2),
                var_tb_sum = round(sum(var_tb), 2),
                var_ti_sum = round(sum(var_ti), 2),
                count = n()) %>%
      ungroup()
    
    all_unitsDF$var_tb_sumP <- round(all_unitsDF$var_tb_sum/helpDF$var_tb_sumAll, 2)
    all_unitsDF$var_ti_sumP <- round(all_unitsDF$var_ti_sum/helpDF$var_ti_sumAll, 2)
    all_unitsDF$countP <- round(all_unitsDF$count/helpDF$countAll, 2)
    
    # columns reorder
    
    all_unitsDF <- all_unitsDF[, c(input$grouping, 'median', 'mean', 'var_tb_sum', 'var_tb_sumP',
                                   'var_ti_sum', 'var_ti_sumP', 'count', 'countP')]
    
    plots_clickDF$main_dTable <- all_unitsDF
    
    
    
  })
  
  observeEvent(input$marginal2_click, {
    
    df <- dfToPlot$df
    
    unit_levels <- levels(as.factor(df[input$grouping][[input$grouping]]))
    unit_clicked <- unit_levels[round(input$marginal2_click$x)]
    
    plots_clickDF$clicked_unit <- unit_clicked
    plots_clickDF$clicked_YM <- 'All months'
    
    # for main datatable
    
    all_unitsDF <- pass_df()
    
    helpDF <- all_unitsDF %>%
      summarise(var_tb_sumAll = sum(var_tb),
                var_ti_sumAll = sum(var_ti),
                countAll = n())
    
    all_unitsDF <- all_unitsDF %>%
      group_by_(.dots = lapply(input$grouping, as.symbol)) %>%
      summarise(median = round(median(var_ub), 2),
                mean = round(mean(var_ub), 2),
                var_tb_sum = round(sum(var_tb), 2),
                var_ti_sum = round(sum(var_ti), 2),
                count = n()) %>%
      ungroup()
    
    all_unitsDF$var_tb_sumP <- round(all_unitsDF$var_tb_sum/helpDF$var_tb_sumAll, 2)
    all_unitsDF$var_ti_sumP <- round(all_unitsDF$var_ti_sum/helpDF$var_ti_sumAll, 2)
    all_unitsDF$countP <- round(all_unitsDF$count/helpDF$countAll, 2)
    
    # columns reorder
    
    all_unitsDF <- all_unitsDF[, c(input$grouping, 'median', 'mean', 'var_tb_sum', 'var_tb_sumP',
                                   'var_ti_sum', 'var_ti_sumP', 'count', 'countP')]
    
    plots_clickDF$main_dTable <- all_unitsDF
    
  })
  
  output$all_units_statsYM <- DT::renderDataTable({
    
    req(input$units, plots_clickDF$main_dTable)
    
    df <- plots_clickDF$main_dTable
    #cat(file = stderr(), h)
    
    rowToPreselect <- match(plots_clickDF$clicked_unit, unique(df[input$grouping][[input$grouping]]))
    
    DT::datatable(df, extensions = c('Scroller', 'ColReorder', 'FixedColumns'),
                  selection = list(target = 'cell', 
                                   selected = matrix(c(rowToPreselect, 1), ncol =2)),
                  caption = paste(plots_clickDF$clicked_unit, ', ', plots_clickDF$clicked_YM),
                  options = list(
                    #deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE,
                    colReorder = TRUE,
                    scrollX = TRUE,
                    fixedColumns = list(leftColumns = 2)#,
                    # rowCallback = DT::JS('function(row, data) {if (data[1] == "China")$("td", row).css("background", "orange")
                    #                 }'))) %>%
                    #rowCallback = DT::JS(h))) %>%
                  )) %>%
      formatPercentage(c('var_tb_sumP', 'var_ti_sumP', 'countP')) %>%
      formatStyle('var_tb_sumP',
                  background = styleColorBar(df$var_tb_sumP, 'lightblue')) %>%
      formatStyle('var_ti_sumP',
                  background = styleColorBar(df$var_ti_sumP, 'lightblue')) %>%
      formatStyle('countP',
                  background = styleColorBar(df$countP, 'lightblue')) %>%
      formatStyle('mean', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue'))) %>%
      formatStyle('median', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue'))) %>%
      formatStyle('var_tb_sum',
                  background = styleColorBar(df$var_tb_sum, '#ffdb99')) %>%
      formatStyle('var_ti_sum',
                  background = styleColorBar(df$var_ti_sum, '#ffdb99')) %>%
      formatStyle('count',
                  background = styleColorBar(df$count, '#ffdb99'))
    
    
    #df
    
  })
  
  # output$main_summary <- renderTable({
  #   
  #   req(input$units, plots_clickDF$summary)
  #   
  #   plots_clickDF$summary
  #   
  # })
  
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
    df[input$grouping] <- as.factor(df[[input$grouping]])
    df$USER_NAME <- as.factor(df$USER_NAME)
    df$COUNTRY_NAME <- as.factor(df$COUNTRY_NAME)
    
    if (!is.null(input$inputs_brush) & !is.null(input$units)){
      
      renderDT <- DT::datatable(df,
                                caption = 'Users monthly utilization',
                                filter = 'top',
                                extensions = c('Buttons', 'FixedHeader', 'ColReorder', 'FixedColumns', 'Responsive'),
                                options = list(dom = 'Blftip',
                                               pageLength = 25,
                                               fixedHeader = TRUE,
                                               colReorder = TRUE,
                                               fixedColumns = list(leftColumns = 2),
                                               buttons = list('copy', 'print', list(extend = 'collection',
                                                                                    buttons = c('csv', 'excel', 'pdf'),
                                                                                    text = 'Download')))) %>%
        formatStyle('var_ub', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue'))) %>%
        formatStyle('var_tb', background = styleColorBar(df$var_tb, 'lightblue')) %>%
        formatStyle('var_ti', background = styleColorBar(df$var_tb, 'lightblue'))
      
      renderDT
      
    }
    
    
    
  })
  
 
  
  output$users_dyg <- renderDygraph({
    
    req(input$user_list_select)
    
    if (!is.null(input$user_list_select)){
      
      f <- df_util %>% 
         filter(USER_NAME %in% input$user_list_select) %>%
         group_by(USER_NAME, YEARMONTH) %>%
         summarise(var_tb = first(var_tb), 
                   var_ti = first(var_ti), 
                   var_eb = first(var_eb),
                   var_ub = first(var_ub)) %>%
          select(YEARMONTH, USER_NAME, var_ub) %>%
          ungroup()
      
     
      df_toDygraph <- spread(data = f, key = USER_NAME, value = var_ub)
      
      df_toDygraph_xts <- xts(x = df_toDygraph[, colnames(df_toDygraph) != 'YEARMONTH'], 
                              order.by = as.POSIXct(strptime(as.character(df_toDygraph$YEARMONTH), format = "%Y-%m-%d")))
      
      plot_dyg <- dygraph(df_toDygraph_xts, y = 'User var_ub') %>% 
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
    
    # df <- dataMap$df@data[!is.na(dataMap$df@data$radius), c('name', 'measure_color', 'measure_circle', 'radius', 'maxc')]
    # arrange(df, desc(radius))
    
  })
  
  output$test3 <- renderPrint({
    
    #clickedCountry()
    
    #userCountry_map()
    
    # dfcirc$df
    
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
      
      if (input$map_statistic == 'quantile'){
        
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
        
        if (input$circle_statistic == 'quantile'){
          
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
      datCircles$radius <- dat$radius #(dat$radius/(abs(datCircles$y)^(2/5))) * 10 # stupid, need adjust circles somehow or reproject somehow, plotted radius differs when changing lat
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
    d <- dat[dat$COUNTRY_NAME == clickedCountry(),]
    
    DT::datatable(d,
                  extensions = c('ColReorder', 'Scroller'),
                  options = list(dom = 'fti',
                                 deferRender = TRUE,
                                 colReorder = TRUE,
                                 scrollY = 400,
                                 scroller = TRUE)) %>%
      formatStyle('var_ub', fontWeight = 'bold', color = styleInterval(c(0.8), c('red', 'blue')))
    
  })
  
  output$timeline <- renderTimevis({
    
    df <- data.frame(
      id = c(1, 2, 3, 4, 5),
      start = c('2016-07-26', '2016-05-26', '2016-05-25', '2016-03-03', '2016-01-25'),
      end = c(NA, NA, NA, NA, NA),
      content = c('<ul>
                  <li>Merge of Queue 1 and 5</li>
                  <li>Move IGF ECM workload from Queue 5 to Queue 3</li>
                  <li>Queue 2 share workload with Queue 3</li>
                  <li>Queue 6 share IGF Portal 8 workload with Queue 3</li>
                  <li>New Queue 1 EMEA Smarter computing project move to Queue 3</li>
                </ul>',
                  
                  'Watermark script announcement',
                  
                  '<ul>
                    <li>Katka on board announcement</li>
                    <li>Team reporting structure changes</li>
                  </ul>',
                  
                  'Global tech lead team changes announcement',
                  
                  'Alignment of WASO with DA and DSG')
    )
    
    timevis(df)
    
  })
  
})

#########################################################################c("#33B83E", "#0B54C2", "darkorchid3")

# PREZENTACNE UCELY, ZMENIT

#   DEPT_NAME
#   ORG_NAME
#   USER_NAME
#   normalizovat var_tb, var_ti, var_eb, var_ub a zmenit nazvy (metric1 - 4)
#   zrusit timevis
#   dorobit sekciu "Description and help"

# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test
# http://stackoverflow.com/questions/20060949/ggplot2-multiple-sub-groups-of-a-bar-chart
# http://www.hafro.is/~einarhj/education/ggplot2/scales.html
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# http://www.cookbook-r.com/
# http://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
