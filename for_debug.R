library(ggplot2)
library(plotly)
library(ggExtra)
library(dplyr)
library(shiny)
library(lubridate)

loadData <- function() {

  mqt_util <- mqt_utilization
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], as.character)
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], factor)
  
  mqt_util$YEAR <- as.POSIXct(strptime(paste0(as.character(mqt_util$YEAR), '0101'), format = '%Y%m%d'))
  mqt_util$YEARMONTH <- as.POSIXct(strptime(paste0(as.character(mqt_util$YEARMONTH), '01'), format = '%Y%m%d'))

}

util_ex <- 
ggplot(aes(x = factor(YEARMONTH), 
           y = util_bill), 
       data = mqt_util %>% 
         filter(SUMMARY == 41 & YEAR >= '2015-01-01') %>% 
         group_by(YEARMONTH,USER_ID) %>% 
         summarise(count = n(), 
                   t_bill = sum(TRACKED_BILLABLE), 
                   t_inv = sum(TRACKED_INVESTMENT), 
                   exp_bill = sum(EXPECTED_BILLABLE)) %>% 
         mutate(util_bill = (t_bill + t_inv)/exp_bill) %>% 
         filter(util_bill > 0) %>%
         ungroup()) + 
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.5) + 
  geom_line(aes(group = CENTER_ID, color = CENTER_ID), 
            size = 1,
            data = mqt_util %>% 
              filter(SUMMARY == 44 & YEAR >= '2015-01-01') %>% 
              group_by(CENTER_ID, YEARMONTH) %>% 
              summarise(count = n(), 
                        t_bill = sum(TRACKED_BILLABLE), 
                        t_inv = sum(TRACKED_INVESTMENT), 
                        exp_bill = sum(EXPECTED_BILLABLE)) %>% 
              mutate(util_bill = (t_bill + t_inv)/exp_bill) %>% 
              ungroup()) +
  theme(panel.background = element_rect(fill =NA),
        panel.grid.major = element_line(colour = '#F6F6F6'),
        axis.line = element_line(colour = '#BDBDBD')) + 
  stat_summary(fun.y = mean, geom = 'point', shape = 1) + 
  geom_hline(yintercept = 0.85, color = 'red', linetype = 2) + 
  xlab('Yearmonth') +
  ggtitle('User utilization')

ggMarginal(util_ex, type = 'histogram', margins = 'y', fill = '#F79420', bins = 50)

# http://stackoverflow.com/questions/21435139/combine-geom-boxplot-with-geom-line