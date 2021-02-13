#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

################################## HEADER CONTENT #######################################

header <- dashboardHeader(title = "ACHD in NSW")

################################## SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar()
    
################################## Body CONTENT #######################################

body <- dashboardBody(valueBox("test","test"))


# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


# Define server logic
server <- function(input, output) { } 







