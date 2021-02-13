#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rgdal)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)

# Path to ACHD database data (note: only accessible at RPAH)
ACHD_path <- '/Users/calumnicholson/Documents/work/HRI/OneDrive - Heart Research Institute/To Do/'
# Path to the study's data folder (note: only accessible at RPAH)
study_folder <- '/Users/calumnicholson/Documents/work/HRI/OneDrive - Heart Research Institute/To Do/'

################################## DATA PREP ############################################

#import ACHD data
achd <- readRDS(file = paste(study_folder, 'output/rpah_analysis_dataset_2020-10-18.rds', sep=""))

################################## HEADER CONTENT #######################################

header <- dashboardHeader(title = "ACHD in NSW")

################################## SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Snapshot tab
        menuItem("Snapshot of ACHD", tabName = "snapshot", icon = icon("dashboard")),
        # Location tab
        menuItem("Location of Patients", tabName = "locations", icon = icon("th")),
        
        # Global Filters
        h4("Global Filters"),
        checkboxGroupInput("sb.bethesda", "Disease severity",
                           choices = c('Simple' = 1,
                                       'Moderate' = 2,
                                       'Complex' = 3,
                                       'Unknown' = 4),
                           selected = c(1, 2, 3)),
        checkboxGroupInput("sb.mortality", "Deceased patients",
                           choices = c('Alive' = 0,
                                       'Deceased' = 1),
                           selected = c(0)),
        dateRangeInput("sb.dates", "Select a time period:",
                       start = "2000-01-01", end = "2020-12-31"),
        sliderInput('sb.age', 'Age',
                    min = 18,
                    max = 110,
                    value = c(18, 110),
                    round = TRUE),
        sliderInput('sb.last.clinic', 'Time since last clinic visit',
                    min = 0,
                    max = 10,
                    value = c(0,10),
                    round = TRUE),
        actionButton("sb.update", "Update"),
        actionButton("sb.reset", "Reset")
        
        
    )
)

################################## Body CONTENT #######################################


body <- dashboardBody(
    tabItems(tabItem(tabName = "snapshot",
                     #Title bar
                     fluidRow(
                         valueBox("Snapshot of ACHD Patients in NSW",
                                  "An overview of the ACHD patients in NSW; 
                                 Their basic demographics, what diagnoses they and how many?",
                                  width = 12,
                                  color = 'olive')
                     ),
                     # Summary Values
                     fluidRow(valueBoxOutput('pt.count', width = 3),
                              valueBoxOutput('simple.count', width = 3),
                              valueBoxOutput('moderate.count', width = 3),
                              valueBoxOutput('complex.count', width = 3)
                     ),
                     # Plots
                     fluidRow(uiOutput("age_box"),
                              uiOutput("sex_box")
                     ),
                     fluidRow(
                         uiOutput("ndx_box"),
                         uiOutput("nclinic_box")
                     ),
                     fluidRow(
                         uiOutput("dx_box")
                     )
        )
        
    )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ############################# GLOBAL FILTERS #################################
    
    achd.filtered <- eventReactive(input$sb.update, {
        achd %>% 
            filter(death %in% input$sb.mortality) %>%
            filter(bethesda_code %in% input$sb.bethesda) %>%
            filter(as.numeric(age, 'years') >= input$sb.age[1]) %>%
            filter(as.numeric(age, 'years') <= input$sb.age[2])
    }) 
    
    dx.count <- reactive({
        # all the diagnosis variables
        dx_names <- c('ABNAO','AbnAV','AbnMV','ABNPV','AbnRV','ABNTV','AbnCA','AbnVeins','AbsentPA','ALCAPA', 'Aneurism',
                      'APwindow','AVfistula','CHB','CorTriatriatum','Gerbodde','Hemi_truncus','LPA_sling','PAV_Malform',
                      'Shones','AoC','AoInterrupt','AR','AS','ASD','AVSD','BAVD','ccTGA','DEXTROCARDIA','DILV','DIRV','DORV',
                      'Ebstein','EISENMENGER','FONTAN','HLHS','LVNONCOMPACT','MAPCA','Other','PA','PAPVD','PAS','PDA','PFO',
                      'PS','SubAS','SubPS','SupraAS','SupraMS','SupraPS','TA','TAPVD','TGA','TOF','TRUNCART','UnkDx','VSD')
        
        # created a dataset with only the disagnoses
        dx.df <- achd.filtered() %>%
            select(dx_names) %>%
            mutate_all(as.character) %>%
            mutate_all(as.numeric) %>%
            summarise_all(sum, na.rm = TRUE) %>%
            t() %>% as.data.frame()
        
        dx.df$HeaderName <- row.names(dx.df)
        dx.df
    })
    
    ############################# SNAPSHOTS TAB #################################
    
    #---------------Value Boxes-------------------#
    # Value boxes with the filtered data

    output$pt.count <- renderValueBox({
        valueBox(achd.filtered() %>% nrow(),
                 'selected patients', 
                 width = 3, color = 'light-blue')
        })

    

    output$simple.count <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 1) %>% nrow(), 
                 "Patients with Simple CHD", 
                 color = 'light-blue')
    })

    

    output$moderate.count <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 2) %>% nrow(), 
                 "Patients with Moderate CHD", 
                 color = 'light-blue')
    })

  
    output$complex.count <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 3) %>% nrow(), 
                 "Patients with Complex CHD", 
                 color = 'light-blue')
    })

    
    #---------------Ploting-----------------------#
    # Age histogram
    observeEvent(input$sb.update, {
                 output$age_box <- renderUI({
        box(plotOutput("age_plot"), title = "Distribution of Patient Ages")
    })
    })

    output$age_plot <- renderPlot({
        ggplot(achd.filtered(), aes(x=as.numeric(age, "years"))) +
            geom_histogram(fill = "light grey", color = "black") +
            labs(y = "Number of Patients", 
                 x = "Age (yrs)") +
            theme(axis.title.x=element_blank()) +
            theme_minimal()
    })

    
    # Sex bar plot

    observeEvent(input$sb.update, {
        output$sex_box <- renderUI({
            box(plotOutput("sex_plot"), title = "Sex of Patients")
        })
    })
    
    
    output$sex_plot <- renderPlot ({
        achd.filtered() %>% filter(!is.na(sex)) %>%
            ggplot(aes(x=sex)) +
            geom_bar(fill = "light grey", color = "black") +
            scale_x_discrete(name = NA,
                             labels=c("1" = "Male", "2" = "Female",
                                      "4" =  "Neither Female or Male")) +
            labs(y = "Number of Patients") +
            theme(axis.title.x=element_blank()) +
            theme_minimal()
    })
    
    # Number of Diagnoses
    observeEvent(input$sb.update, {
        output$ndx_box <- renderUI({
            box(plotOutput("ndx_plot"), title = "Number of CHD Diagnoses per Patient")
        })
    })
    
    output$ndx_plot <- renderPlot ({
        ggplot(achd.filtered(), aes(x=as.integer(no_dx)) ) +
            geom_bar(fill = "light grey", color = "black") +
            scale_x_continuous() +
            labs(x = "Number of Diagnoses", 
                 y = "Number of Patients") +
            theme() +
            theme_minimal() 
    })

    
    # Number of Clinic Visits
    observeEvent(input$sb.update, {
        output$nclinic_box <- renderUI({
            box(plotOutput("nclinic_plot"), title = "Number of Clinic Visits per Patient")
        })
    })

    output$nclinic_plot <- renderPlot ({
        ggplot(achd.filtered(), aes(x=no_clinics)) +
            geom_bar(fill = "light grey", color = "black") +
            scale_x_continuous() +
            labs(x = "Number of Clinic Visits", 
                 y = "Number of Patients") +
            theme() +
            theme_minimal() 
    })
    
    ### Frequency of Diagnoses
    observeEvent(input$sb.update, {
        output$dx_box <- renderUI({
            box(plotOutput("dx_plot", height = 750), 
                title = "Frequency of Each Diagnosis",
                width = 12, height = 825)
        })
    })
    
    output$dx_plot <- renderPlot({
        ggplot(dx.count(), aes(x = HeaderName, y=V1)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(title = "Frequecy of each diagnosis", 
                 y = "count",
                 x = "") +
            coord_flip()
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
