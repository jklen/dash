
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggExtra)

# data load and prepare

load('.Rdata')

shinyServer(function(input, output) {
  
    
  output$reac_units <- renderUI({
    
    units_list <- pass_units()
      
    checkboxGroupInput(inputId = 'units',
                       label = input$grouping,
                       choices = units_list
                       )
      
  })
  
  pass_units <- reactive({
    
    if (input$grouping == 'Geo'){
      units <- unique(df_util$GEO_NAME)
    } else {
      
      if (input$grouping == 'Organization'){
        units <- unique(df_util$ORG_NAME) 
      } else {
        
        if (input$grouping == 'Department'){
          units <- unique(df_util$DEPT_NAME) 
        } else {
          units <- unique(df_util$USER_NAME) 
        }
      }
    }

  units  
    
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
  #             - boxplot alebo ciara (celkova utilizacia, priemer alebo median)
  #             - zoom na mesiace aj hodnoty
  #             - druhy graf pod celkovy histogram alebo boxplot, podla zvolenych
  #                 centier, dept., userov, zoradeny podla medianov
  #             - checkbox na centra, departmenty, alebo userov
  #             - checkbox ci budu prompty kaskadovane
  #             * globalna utilizacia
  #             - cislo pri boxplote: expected billable hours
  #             - cislo pri boxplote: rast v % celkovej utilizacie oproti
  #                 minulemu mesiacu
  #             - cislo pri boxplote: percento ludi s utilizaciou = 0?
  #             x checkbox na utilizacia = 0
  #             - listbox na typ marginal plotu
# analyticka funkcionalita - rozne metriky na task?
  # 1 premenna - grafy, summary
  # 2 a viac premennych
  # upload suboru
# vizualizacne vychytavky - rCharts, Google charts (google vis), plotly, ggvis, ggplot2
  # sankey diagram - budget a spending projektov?
  # interaktivna mapa, nejaka jednoducha metrika na staty

# v tabulke user je v 34 riadku volaka cinska picovina, robi bordel - stlpec BUSNEED