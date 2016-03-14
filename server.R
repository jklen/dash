
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
    
    cat(file=stderr(), "selected grouping - ", input$grouping) # vypise do konzoly

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
  #             - hlavny graf - boxplot
  #             - pre boxplot - checkbox, ci ukazat box pre 1 level nad (pre porovnanie napr. danej organizacie a gea pod ktorym je),
  #                 nebude zahrnuty do marginal plotu
  #             - brushing  bez zoomu ako filter do tabulky pod - utilizacia userov
  #             - slider mesiace aj hodnoty
  #             - druhy graf pod - celkovy histogram alebo boxplot (listbox), podla zvolenych
  #                 centier, dept., userov, zoradeny podla medianov
  #             - marginal plot na to co je zobrazene celkovo (selectnute v promtoch), compedium alebo http://daattali.com/shiny/ggExtra-ggMarginal-demo/
  #             - listbox na typ marginal plotu
  #             * checkbox na centra, departmenty, alebo userov
  #             - checkbox ci budu prompty kaskadovane
  #             - cislo pri boxplote: expected billable hours
  #             - cislo pri boxplote: rast v % celkovej utilizacie oproti
  #                 minulemu mesiacu
  #             - download suboru - podla hlavneho grafu
  #             - checkbox na utilizacia = 0

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