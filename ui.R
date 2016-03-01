
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))

library(shiny) # load shiny at beginning at both scripts

shinyUI(pageWithSidebar( # standard shiny layout, controls on the
  # left, output on the right
  
  headerPanel("Minimal example"), 
  sidebarPanel( 
    textInput(inputId = "plot_name",
              label = "plot name???", 
              value = "kok" # initial value
    ),
    sliderInput(inputId = 'obs',
                label = 'Observations',
                value = 2000,
                min = 1000,
                max = 10000,
                step = 100,
                ticks = T,
                sep = ''
                
    )
  ),
  
  mainPanel( # all of the output elements go in here
    h3("This is you saying it"), # title with HTML helper
    plotOutput("plotDisplay"),
    textOutput("textDisplay")# this is the name of the output
    # element as defined in server.R
  )
))