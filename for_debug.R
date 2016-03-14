library(ggplot2)
library(plotly)
library(ggExtra)
library(dplyr)
library(shiny)
library(lubridate)

loadData <- function() {
  
  mqt_utilization <- read.csv('mqt_utilization.csv')
  dept <- read.csv('dept.csv')
  org <- read.csv('org.csv')
  geo <- read.csv('geo.csv')
  user <- read.table('user.tsv', sep = '|', header = T)
  
  dept <- dept %>%
    select(ID, NAME) %>%
    rename(DEPT_ID = ID, DEPT_NAME = NAME)
  org <- org %>%
    select(ID, NAME) %>%
    rename(ORG_ID = ID, ORG_NAME = NAME)
  geo <- geo %>%
    select(ID, NAME) %>%
    rename(GEO_ID = ID, GEO_NAME = NAME)
  user <- user %>%
    mutate(USER_NAME = paste(FIRSTNAME, LASTNAME)) %>%
    rename(USER_ID = ID) %>%
    select(USER_ID, USER_NAME)
  
  mqt_util <- mqt_utilization
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], as.character)
  mqt_util[,c(1:5, 10:13, 27)] <- lapply(mqt_util[,c(1:5, 10:13, 27)], factor)
  
  mqt_util$YEAR <-
    as.POSIXct(strptime(paste0(as.character(mqt_util$YEAR), '0101'), format = '%Y%m%d'))
  
  mqt_util$YEARMONTH <-
    as.POSIXct(strptime(paste0(as.character(mqt_util$YEARMONTH), '01'), format = '%Y%m%d'))
  
  dept$DEPT_ID <- factor(dept$DEPT_ID)
  org$ORG_ID <- factor(org$ORG_ID)
  geo$GEO_ID <- factor(geo$GEO_ID)
  user$USER_ID <- factor(user$USER_ID)
  dept$DEPT_NAME <- as.character(dept$DEPT_NAME)
  org$ORG_NAME <- as.character(org$ORG_NAME)
  geo$GEO_NAME <- as.character(geo$GEO_NAME)
  user$USER_NAME <- as.character(user$USER_NAME)
  
  # used groupings
  
  df_util <- mqt_util %>%
    filter(SUMMARY == 41) %>%
    group_by(GEO_ID, DEPT_ID, ORG_ID, USER_ID, YEARMONTH) %>%
    summarise(t_bill = sum(TRACKED_BILLABLE), 
              t_inv = sum(TRACKED_INVESTMENT), 
              exp_bill = sum(EXPECTED_BILLABLE)) %>% 
    mutate(util_bill = (t_bill + t_inv)/exp_bill) %>% 
    inner_join(dept, by = 'DEPT_ID') %>%
    inner_join(org, by = 'ORG_ID') %>%
    inner_join(geo, by = 'GEO_ID') %>%
    inner_join(user, by = 'USER_ID') %>%
    select(GEO_ID, GEO_NAME, DEPT_ID, DEPT_NAME, ORG_ID, ORG_NAME, USER_ID, USER_NAME,
           YEARMONTH, t_bill, t_inv, exp_bill, util_bill) %>%
    filter(util_bill > 0) %>%
    ungroup()
  
  # 237 userov na yearmonth bolo v roznych departmentoch
  
  return(df_util)
  
}

df_util <- loadData()

# http://stackoverflow.com/questions/21435139/combine-geom-boxplot-with-geom-line
# https://github.com/rstudio/shiny/issues/678