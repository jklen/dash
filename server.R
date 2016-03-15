
library(shiny)
library(ggplot2)
library(dplyr)
library(ggExtra)

# data load and prepare

load('.Rdata')

shinyServer(function(input, output) {
  
  # rendering checkbox group based on reactive vector of units
  
  output$reac_units <- renderUI({
    
    units_list <- pass_units()
      
    checkboxGroupInput(inputId = 'units',
                       label = input$grouping,
                       choices = units_list
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
        
        if (input$grouping == 'DPT_NAME'){
          units <- unique(df_util$DEPT_NAME) 
        } else {
          units <- unique(df_util$USER_NAME) 
        }
      }
    }
    
    cat(file=stderr(), "selected grouping - ", input$grouping) # vypise do konzoly

  units  
    
  })
  
  # creating reactive df based on date, value, selected units
  
  pass_df_util <- reactive({
    
    df_util_reac <- df_util[between(df_util$YEARMONTH, as.POSIXct(input$date_range[1]), as.POSIXct(input$date_range[2])),]
    
    df_util_reac <- df_util_reac[between(df_util_reac$util_bill, input$util_value[1], input$util_value[2]),]
    
    if (!is.null(pass_units())){
    
      df_util_reac <- ifelse(input$grouping == 'Geo', df_util_reac[df_util_reac$GEO_NAME %in% pass_units(),],
                        ifelse(input$grouping == 'Organization', df_util_reac[df_util_reac$ORG_NAME %in% pass_units(),],
                          ifelse(input$grouping == 'Department', df_util_reac[df_util_reac$DEPT_NAME %in% pass_units(),],
                                 df_util_reac[df_util_reac$USER_NAME %in% pass_units(),]
                          )
                        )
                      )
    }
    
    # group reactive df based on selected grouping
    
    groups <- ifelse(input$grouping == 'USER_NAME', c('YEARMONTH', 'USER_NAME'), c(input$grouping, 'YEARMONTH', 'USER_NAME'))
    
    df_util_reac <- df_util_reac %>% 
      group_by_(.dots = lapply(groups, as.symbol)) %>%
      summarise(t_bill = sum(t_bill), 
                t_inv = sum(t_inv), 
                exp_bill = sum(exp_bill)) %>%
      mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
      ungroup()
    
    # if selected, append data from level above (except for geo)
    
    if (input$check_uplevel == T){
      
      
      
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
  #             - hlavny graf - boxplot
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