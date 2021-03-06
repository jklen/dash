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
  countries <- read.csv('countries.csv')
  
  dept <- dept %>%
    select(ID, NAME) %>%
    rename(DEPT_ID = ID, DEPT_NAME = NAME)
  org <- org %>%
    select(ID, NAME) %>%
    rename(ORG_ID = ID, ORG_NAME = NAME)
  geo <- geo %>%
    select(GEO_ID, GEO_NAME) #%>%
    #rename(GEO_ID = ID, GEO_NAME = NAME)
  countries <- countries %>%
    select(COUNTRY_ID, COUNTRY_NAME) 
  user <- user %>%
    mutate(USER_NAME = paste(FIRSTNAME, LASTNAME)) %>%
    rename(USER_ID = ID) %>%
    select(USER_ID, USER_NAME, COUNTRY) %>%
    rename(COUNTRY_ID = COUNTRY)
  
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
  countries$COUNTRY_ID <- factor(countries$COUNTRY_ID)
  user$USER_NAME <- as.character(user$USER_NAME)
  user$COUNTRY_ID <- factor(user$COUNTRY_ID)
  
  # used groupings (41 - je tam aj rola)
  
  user <- user %>% inner_join(countries, by = 'COUNTRY_ID')
    
  
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
    #inner_join(user_country, by = 'USER_ID') %>%
    select(GEO_ID, GEO_NAME, DEPT_ID, DEPT_NAME, ORG_ID, ORG_NAME, USER_ID, USER_NAME, COUNTRY_ID, COUNTRY_NAME,
           YEARMONTH, t_bill, t_inv, exp_bill, util_bill) %>%
    #filter(util_bill > 0) %>%
    filter(!is.na(util_bill)) %>%
    ungroup()
  
  # 237 userov na yearmonth bolo v roznych departmentoch
  
  return(df_util)
  
}

df_util <- loadData()

###################################### some testing

lnd <- readOGR(dsn = 'C:\\Users\\IBM_ADMIN\\Desktop\\R\\worldBordersMap', layer = 'ne_110m_admin_0_countries')
wrldMap <- map('world', plot = F, fill = T)

lnd@data <- lnd@data %>% left_join(countries_mean)

pal <- colorNumeric(palette = 'Blues', domain = lnd$cmean) # definovanie palety vid. dokumentaciu

leaflet() %>% addPolygons(data = lnd, stroke = F, fillOpacity = 0.4, smoothFactor = 0.2, color = ~pal(cmean)) %>% addTiles()
leaflet(data = wrldMap) %>% addPolygons() %>% addTiles()

lnd[SpatialPoints(coords = matrix(c(19.50766, 48.72671), ncol = 2), proj4string = CRS(proj4string(lnd))), ]@data$name # nazov krajiny podla coordinatov

# http://stackoverflow.com/questions/21435139/combine-geom-boxplot-with-geom-line
# https://github.com/rstudio/shiny/issues/678
# http://deanattali.com/2015/03/29/ggExtra-r-package/
# http://shinyapps.org/apps/RGraphCompendium/index.php
# http://stackoverflow.com/questions/21388845/ggplot-arranging-boxplots-of-multiple-y-variables-for-each-group-of-a-continuou
# https://nsaunders.wordpress.com/2013/02/26/rggplot2-tip-aes_string/
# http://stackoverflow.com/questions/21468380/overlay-geom-points-on-geom-boxplotfill-group
# http://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
# http://stackoverflow.com/questions/6667091/pivot-table-like-output-in-r alebo DPLYR, TIDYR + datatables? pre crosstaby
# http://rstudio.github.io/DT/ - datatables
# https://gallery.shinyapps.io/105-plot-interaction-zoom/
# http://stackoverflow.com/questions/11028353/passing-string-variable-facet-wrap-in-ggplot-using-r
# http://stackoverflow.com/questions/13445753/force-ggplot2-scatter-plot-to-be-square-shaped - pri facetingu funguje theme
# http://stackoverflow.com/questions/23224142/converting-data-frame-to-xts-order-by-requires-an-appropriate-time-based-object xts akceptuje Date, timeDate, POSIXct
# http://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb
# http://stackoverflow.com/questions/29065871/interactive-map-with-leafletr-variable-point-size legenda na point size - trocha komplikovane
# http://stackoverflow.com/questions/20251119/increase-the-size-of-variable-size-points-in-ggplot2-scatter-plot
# http://stackoverflow.com/questions/20254084/plot-normal-left-and-right-skewed-distribution-in-r
# http://r.789695.n4.nabble.com/Bimodal-distribution-td1567430.html create bimodal distribution
# http://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames - different layers, different data
# http://www.r-bloggers.com/r-using-rcolorbrewer-to-colour-your-figures-in-r/
# http://stackoverflow.com/questions/7466023/how-to-give-color-to-each-class-in-scatter-plot-in-r - colors, last answer
# http://bioinfo.umassmed.edu/bootstrappers/bootstrappers-courses/courses/rCourse/resources/helpfulGuides/Rcolorstyle.pdf - guide to using colors
# http://research.stowers-institute.org/efg/Report/UsingColorInR.pdf - colors in R basics presentation
# http://stackoverflow.com/questions/10210450/ggplot2-set-alpha-0-for-certain-points-depending-on-fill-value
# http://stackoverflow.com/questions/5577727/is-there-an-r-function-for-finding-the-index-of-an-element-in-a-vector
# https://yihui.shinyapps.io/DT-proxy/ datatable proxy, nullify selected rows/columns

#### data read for predictive modeling presentation


q2 <- read.csv('C:\\Users\\IBM_ADMIN\\Desktop\\R\\DATAFILES\\q2.csv')
q2 <- q2 %>%
  select(week_start_monday, tracked_q2_roleWB, tracked_q2_roleWB_predicted) %>%
  filter(week_start_monday != '2016-06-12') %>%
  rename(TimeLabel_EVENT = week_start_monday, forecasted = tracked_q2_roleWB_predicted, measured = tracked_q2_roleWB)

q2$geo_queue_model <- 'Q2'

handshake_input <- read.csv('C:\\Users\\IBM_ADMIN\\Desktop\\R\\DATAFILES\\handshake_input.csv')
handshake_input <- handshake_input %>%
  select(TimeLabel_EVENT, geo_queue_model, QA_WPL_step_tracked_next_week_forecast, QA_WPL_step_tracked) %>%
  rename(forecasted = QA_WPL_step_tracked_next_week_forecast, measured = QA_WPL_step_tracked)

handshake_input$geo_queue_model <- as.character(handshake_input$geo_queue_model)

handshake_input <- handshake_input %>%
  bind_rows(q2) %>%
  filter(geo_queue_model %in% c('6-8-1', '6-8-2', '6-5-1', '6-1-1', '5-8-1', '5-5-2', '5-5-1',
                                '5-2-3', '5-2-2', '5-2-1', '5-1-2', '5-1-1', '2-91-1', '2-2-2',
                                '2-2-1', '1-2-1', 'Q2')) %>%
  filter(TimeLabel_EVENT != '2016-06-12')

handshake_input <- xts(x = handshake_input[,colnames(handshake_input) != 'TimeLabel_EVENT'], order.by = as.POSIXct(strptime(as.character(handshake_input$TimeLabel_EVENT), format = '%Y-%m-%d')))

####