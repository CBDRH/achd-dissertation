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

sa2.polys <- readOGR('./data/shape_files/sa2_polys.shp')
sa3.polys <- readOGR('./data/shape_files/sa3_polys.shp')
sa4.polys <- readOGR('./data/shape_files/sa4_polys.shp')

## These will porbably be reactive functions in the future, but for now jsut some filtering
snapshot <- achd %>% filter(death != 1)

# summary values header to be displayed on every page

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
                                       'Complex' = 3),
                           selected = c(1, 2, 3)),
        checkboxGroupInput("sb.mortality", "Deceased patients",
                           choices = c('Alive' = 'alive',
                                       'Deceased' = 'dead',
                                       'Both' = 'both'),
                           selected = c('alive')),
        dateRangeInput("sb.dates", "Select a time period:",
                       start = "2000-01-01", end = "2020-12-31"),
        sliderInput('sb.age', 'Age',
                    min = floor(min(as.numeric(achd$age, 'years'))),
                    max = ceiling(max(as.numeric(achd$age, 'years'))),
                    value = c(floor(min(as.numeric(achd$age, 'years'))),
                              ceiling(max(as.numeric(achd$age, 'years')))),
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
    #Snapshot tab content
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
                     fluidRow(valueBox('test',snapshot %>% select(achd_id) %>% nrow()),
                              valueBoxOutput('pt.count'),
                              valueBoxOutput('simple.count'),
                              valueBoxOutput('moderate.count'),
                              valueBoxOutput('complex.count'),
                     ),
                     # Boxes need to be put in a row (or column)
                     fluidRow(
                         box(plotOutput("age_plot"), title = "Distribution of Patient Ages"),
                         box(plotOutput("sex_plot"), title = "Sex of Patients")
                     ),
                     fluidRow(
                         box(plotOutput("ndx_plot"), title = "Number of CHD Diagnoses per Patient"),
                         box(plotOutput("nclinic_plot"), title = "Number of Clinic Visits per Patient")
                     ),
                     fluidRow(
                         box(plotOutput("dx_plot", height = 750), 
                             title = "Frequency of Each Diagnosis",
                             width = 12, height = 825)
                     )
    ),
            # Locations tab content
            tabItem(tabName = "locations",
                    #Title bar
                    fluidRow(
                        valueBox("Patient Locations in NSW",
                                 "Where are patients living throughout NSW 
                                 and what kinds of CHD a present in each area?",
                                 width = 12,
                                 color = 'olive')
                    ),
                    # Summary Values
                    fluidRow(valueBoxOutput('pt.count'),
                             valueBoxOutput('simple.count'),
                             valueBoxOutput('moderate.count'),
                             valueBoxOutput('complex.count'),
                    ),
                    fluidRow(
                        box(leafletOutput('locations.map', height = 550),
                            width = 7, height = 580
                        ),
                        box(h1 = "Area level information will display here on click", 
                            width = 5, height = 580
                        )
                    ),
                    fluidRow(
                        box(title = "Map Customisation", width = 9, height = 70,  solidHeader = TRUE,
                                 "Adust the following options and then click 'Update'
                                 to customise the map, click 'Reset' to return to the default options"
                        ),
                        box(width = 3, height = 70, solidHeader = FALSE,
                            actionButton("loc.map.update", "Update"),
                            actionButton("loc.map.reset", "Reset")
                        )
                    ),
                    fluidRow(
                        column(width = 6,
                          box(width = NULL, solidHeader = TRUE,
                              selectInput("asgs.select", "Select an area:",
                                          choices = c("SA2" = "sa2",
                                                      "SA3" = "sa3",
                                                      "SA4" = "sa4"))
                          ),
                          box(width = NULL, solidHeader = TRUE,
                              checkboxGroupInput("bethesda", "Filter by disease severity:",
                                                 choices = c('Simple' = 1,
                                                             'Moderate' = 2,
                                                             'Complex' = 3),
                                                 selected = c(1, 2, 3))
                          ),
                          box(width = NULL, solidHeader = TRUE,
                              checkboxGroupInput("mortality.select", "Filter deceased patients:",
                                                 choices = c('Alive' = 'alive',
                                                             'Deceased' = 'dead'),
                                                 selected = c('alive'))
                          ),
                        ),
                        column(width = 6,
                          box(width = NULL, solidHeader = TRUE,
                              dateRangeInput("clinic.dates", "Select a time period:",
                                             start = "2000-01-01", end = "2020-12-31")
                          ),
                          box(width = NULL, solidHeader = TRUE,
                              sliderInput('age.range', 'Select an age range:',
                                          min = floor(min(as.numeric(achd$age, 'years'))),
                                          max = ceiling(max(as.numeric(achd$age, 'years'))),
                                          value = c(floor(min(as.numeric(achd$age, 'years'))),
                                                    ceiling(max(as.numeric(achd$age, 'years')))),
                                          round = TRUE
                             )
                          ),
                          box(width = NULL, solidHeader = TRUE,
                              sliderInput('last.clinic', 'filter by time since last clinic visit:',
                                          min = 0,
                                          max = 10,
                                          value = c(0,10),
                                          round = TRUE)
                              
                          )
                        )
                    )
                   )
            )
)




# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)


         

# Define server logic
server <- function(input, output) { 
    
    ############################# GLOBAL FILTERS #################################
    
    
    ############################# SNAPSHOTS TAB #################################
    
    #---------------Value Boxes-------------------#
    # Update the value boxes with the filtered data
    output$pt.count <- renderValueBox({
                                valueBox(snapshot %>% select(achd_id) %>% nrow(),
                                         "Patients selected",
                                         width = 3, color = 'light-blue')
    })
    
    output$simple.count <- renderValueBox({
                                valueBox(snapshot %>% filter(bethesda_code == 1) %>% nrow(), 
                                         "Patients with Simple CHD", 
                                         width = 3, color = 'light-blue')
    })
    
    output$moderate.count <- renderValueBox({
                                valueBox(snapshot %>% filter(bethesda_code == 2) %>% nrow(), 
                                         "Patients with Moderate CHD", 
                                         width = 3, color = 'light-blue')
    })
    
    output$complex.count <- renderValueBox({
                                valueBox(snapshot %>% filter(bethesda_code == 3) %>% nrow(), 
                                         "Patients with Complex CHD", 
                                         width = 3, color = 'light-blue')
    })
    
    
    
    #---------------Ploting-----------------------#
    # Age histogram
    output$age_plot <- renderPlot({
        ggplot(snapshot, aes(x=as.numeric(age, "years"))) +
            geom_histogram(fill = "light grey", color = "black") +
            labs(y = "Number of Patients", 
                 x = "Age (yrs)") +
            theme(axis.title.x=element_blank()) +
            theme_minimal()
    })
    
    # Sex bar plot
    output$sex_plot <- renderPlot ({
        snapshot %>% filter(!is.na(sex)) %>%
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
    output$ndx_plot <- renderPlot ({
        ggplot(snapshot, aes(x=as.integer(no_dx)) ) +
            geom_bar(fill = "light grey", color = "black") +
            scale_x_continuous() +
            labs(x = "Number of Diagnoses", 
                 y = "Number of Patients") +
            theme() +
            theme_minimal() 
    })
    
    # Number of Clinic Visits
    output$nclinic_plot <- renderPlot ({
        ggplot(snapshot, aes(x=no_clinics)) +
            geom_bar(fill = "light grey", color = "black") +
            scale_x_continuous() +
            labs(x = "Number of Clinic Visits", 
                 y = "Number of Patients") +
            theme() +
            theme_minimal() 
    })
    
    ### Frequency of Diagnoses
    
    # all the diagnosis variables
    dx_names <- c('ABNAO','AbnAV','AbnMV','ABNPV','AbnRV','ABNTV','AbnCA','AbnVeins','AbsentPA','ALCAPA', 'Aneurism',
                  'APwindow','AVfistula','CHB','CorTriatriatum','Gerbodde','Hemi_truncus','LPA_sling','PAV_Malform',
                  'Shones','AoC','AoInterrupt','AR','AS','ASD','AVSD','BAVD','ccTGA','DEXTROCARDIA','DILV','DIRV','DORV',
                  'Ebstein','EISENMENGER','FONTAN','HLHS','LVNONCOMPACT','MAPCA','Other','PA','PAPVD','PAS','PDA','PFO',
                  'PS','SubAS','SubPS','SupraAS','SupraMS','SupraPS','TA','TAPVD','TGA','TOF','TRUNCART','UnkDx','VSD')
    
    # created a dataset with only the disagnoses
    snapshot.dx <- snapshot %>%
        select(dx_names) %>%
        mutate_all(as.character) %>%
        mutate_all(as.numeric) %>%
        summarise_all(sum, na.rm = TRUE) %>%
        t() %>% as.data.frame()
    
    snapshot.dx$HeaderName <- row.names(snapshot.dx)
    
    output$dx_plot <- renderPlot({
        ggplot(snapshot.dx, aes(x = HeaderName, y=V1)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(title = "Frequecy of each diagnosis", 
                 y = "count",
                 x = "") +
            coord_flip()
    })
    
    
    
    
    #################################### Locations Tab #########################################
    
    
    
    ### Patient Locations Maps ###
    
    output$locations.map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = sa2.polys)
    })
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
