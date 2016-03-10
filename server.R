
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggExtra)

# data load and prepare



loadData <- function() {

  mqt_utilization <- read.csv('mqt_utilization.csv')

  mqt_util <- mqt_utilization
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], as.character)
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], factor)

  mqt_util$YEAR <-
    as.POSIXct(strptime(paste0(as.character(mqt_util$YEAR), '0101'), format = '%Y%m%d'))

  mqt_util$YEARMONTH <-
    as.POSIXct(strptime(paste0(as.character(mqt_util$YEARMONTH), '01'), format = '%Y%m%d'))

  # used groupings

  util_global <- mqt_util %>%
    filter(SUMMARY == 41 & YEAR >= '2015-01-01' & TRACKED_BILLABLE > 0) %>%
    group_by(YEARMONTH) %>%
    summarise(t_bill = sum(TRACKED_BILLABLE),
              t_inv = sum(TRACKED_INVESTMENT),
              exp_bill = sum(EXPECTED_BILLABLE)) %>%
    mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
    ungroup()

  util_center <- mqt_util %>%
    filter(SUMMARY == 44 & YEAR >= '2015-01-01') %>%
    group_by(CENTER_ID, YEARMONTH) %>%
    summarise(t_bill = sum(TRACKED_BILLABLE),
              t_inv = sum(TRACKED_INVESTMENT),
              exp_bill = sum(EXPECTED_BILLABLE)) %>%
    mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
    ungroup()

  util_dept <-
    mqt_util[mqt_util$SUMMARY == 43, c('DEPT_ID', 'TRACKED_BILLABLE', 'TRACKED_INVESTMENT',
                                       'EXPECTED_BILLABLE', 'UTIL_BILLABLE')]

  colnames(util_dept) <- c('DEPT_ID', 't_bill', 't_inv', 'exp_bill', 'util_bill')

  util_user <- mqt_util %>%
    filter(SUMMARY == 41 & YEAR >= '2015-01-01') %>%
    group_by(YEARMONTH,USER_ID) %>%
    summarise(t_bill = sum(TRACKED_BILLABLE),
              t_inv = sum(TRACKED_INVESTMENT),
              exp_bill = sum(EXPECTED_BILLABLE)) %>%
    mutate(util_bill = (t_bill + t_inv)/exp_bill) %>%
    filter(util_bill > 0) %>%
    ungroup()
  
}

# loadData()
load('.Rdata')

main_plot <- ggplot(aes(x = factor(YEARMONTH), 
                        y = util_bill), 
                    data = util_user) + 
  #geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.5) +
  geom_line(aes(group = 'global'), data = util_global, linetype = 2) +
  theme(panel.background = element_rect(fill =NA),
        panel.grid.major = element_line(colour = '#F6F6F6'),
        axis.line = element_line(colour = '#BDBDBD')) +
  stat_summary(fun.y = mean, geom = 'point', shape = 1) +
  geom_hline(yintercept = 0.85, color = 'red', linetype = 2) +
  xlab('Yearmonth')

shinyServer(function(input, output) { # server is defined within
  # these parentheses
  
  output$plotDisplay <- renderPlot({
    
    main_plot1 <- main_plot + geom_line(aes(group = CENTER_ID, color = CENTER_ID),
                      data = util_center[util_center$CENTER_ID %in% input$center,])
    
    ggMarginal(main_plot1, type = 'histogram', margins = 'y', fill = '#F79420', bins = 50)
    
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
  # utilizacia  - podla centra, departmentu, usera
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

