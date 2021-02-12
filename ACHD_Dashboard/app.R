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

#setwd()

# Path to ACHD database data (note: only accessible at RPAH)
pt_data <- '/Users/calumnicholson/Documents/work/HRI/OneDrive - Heart Research Institute/To Do/'

# Path to the study's data folder (note: only accessible at RPAH)
app_data <- './data/'

################################## LOAD DATA ############################################

#import ACHD data
achd <- readRDS(file = paste(pt_data, 'output/rpah_analysis_dataset_2020-10-18.rds', sep=""))

#Import area polygons
sa2.polys <- readOGR(paste(app_data, 'shape_files/sa2_polys.shp', sep=""))
sa3.polys <- readOGR(paste(app_data, 'shape_files/sa3_polys.shp', sep=""))
sa4.polys <- readOGR(paste(app_data, 'shape_files/sa4_polys.shp', sep=""))

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
        checkboxGroupInput("sb.sex", "Sex",
                           choices = c('Male' = 1,
                                       'Female' = 2,
                                       'Neither' = 4),
                           selected = c(1, 2, 4)),
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
    tabItems(
        #---------------Snapshot Tab-------------------#
        tabItem(tabName = "snapshot",
                     #Title bar
                     fluidRow(
                         valueBox("Snapshot of ACHD Patients in NSW",
                                  "An overview of the ACHD patients in NSW; 
                                 Their basic demographics, what diagnoses they and how many?",
                                  width = 12,
                                  color = 'olive')
                     ),
                     # Summary Values
                     fluidRow(valueBoxOutput('pt.count.ss', width = 3),
                              valueBoxOutput('simple.count.ss', width = 3),
                              valueBoxOutput('moderate.count.ss', width = 3),
                              valueBoxOutput('complex.count.ss', width = 3)
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
        ),
    
        #---------------Locations Tab-------------------#
        tabItem(tabName = "locations",
                #Title bar
                fluidRow(
                    valueBox("Patient Locations in NSW",
                             "Where are patients living throughout NSW 
                                     and what kinds of CHD a present in each area?",
                             width = 12,
                             color = 'olive')
                ),
                #Map Customisation 
                fluidRow(
                    box(title = "Map Customisation",
                        selectInput('loc.poly.select', 'Select Area Level:',
                                    choices = c('SA2' = 2,
                                                'SA3' = 3,
                                                'SA4' = 4),
                                    selected = 4),
                        checkboxGroupInput("loc.gcc", "Filter by Region",
                                           choices = c('Greater Sydney' = "Greater Sydney",
                                                       'Rest of NSW' = "Rest of NSW",
                                                       'ACT' = "Australian Capital Territory"),
                                           selected = c("Greater Sydney", "Rest of NSW", "Australian Capital Territory")),
                        actionButton("loc.update", "Update")
                    )
                ),
                # Summary Values
                fluidRow(valueBoxOutput('pt.count.loc', width = 3),
                         valueBoxOutput('simple.count.loc', width = 3),
                         valueBoxOutput('moderate.count.loc', width = 3),
                         valueBoxOutput('complex.count.loc', width = 3)
                ),
                fluidRow(uiOutput('locations.map.box'),
                         uiOutput('locations.summary.box')
                )
        )
    )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ############################# REACTIVE FUNCTIONS #################################
    
    # Global Filters
    achd.filtered <- eventReactive(input$sb.update, {
        achd %>% 
            filter(death %in% input$sb.mortality) %>%
            filter(sex %in% input$sb.sex) %>%
            filter(bethesda_code %in% input$sb.bethesda) %>%
            filter(as.numeric(age, 'years') >= input$sb.age[1]) %>%
            filter(as.numeric(age, 'years') <= input$sb.age[2])
    }) 
    
    # count frequency of diagnoses
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
    
    # Create area-level ACHD data
    set.area.achd <- reactive({
        if (input$loc.poly.select == 2) {
        area.achd <- achd.filtered() %>%
            count(sa2) %>% rename("ACHD_count" = n,
                                  "area" = sa2)
        } else if (input$loc.poly.select == 3) {
            area.achd <- achd.filtered() %>%
                count(sa3) %>% rename("ACHD_count" = n,
                                      "area" = sa3)
        } else {area.achd <- achd.filtered() %>%
                 count(sa4) %>% rename("ACHD_count" = n,
                                       "area"= sa4)}
        area.achd
    })
    
    #select polygons for plotting 
    set.polys <- eventReactive( input$loc.update, {
        # select the correct shapefile
        if (input$loc.poly.select == 2) {polys <- sa2.polys} 
        else if (input$loc.poly.select == 3) {polys <- sa3.polys} 
        else {polys <- sa4.polys}
        
        # add the achd population for each area 
        polys <- merge(polys, set.area.achd(), by.x = 'NAME16', by.y = 'area')
        #convert NAs to 0
        polys@data <- polys@data %>% replace_na(list('ACHD_count' = 0))
        
        # Filter by the GCC areas selected
        polys.filtered <- subset(polys, polys$GCC_NAME16 %in% input$loc.gcc)
        polys.filtered
    })
    
    #Set bins for locations map
    bins.loc <- reactive({
        bins <- seq(0, 
                    plyr::round_any(max(set.polys()$ACHD_count), 10, ceiling), 
                    plyr::round_any(max(set.polys()$ACHD_count), 10, ceiling)/10)
        bins
    })
    
    #Set colour palette for locations map
    pal.loc <- reactive({
        pal <- colorBin("YlOrRd", domain = set.polys()$ACHD_count, bins = bins.loc())
        pal
    })
    
    #Set labels for locations map
    labels.loc <- reactive({
        sprintf("<strong>%s</strong><br/>%g ACHD patients in area",
                 set.polys()$NAME16, set.polys()$ACHD_count) %>% lapply(htmltools::HTML)
    })
    
    ############################# SNAPSHOTS TAB #################################
    
    #---------------Value Boxes-------------------#
    # Value boxes with the filtered data
    
    output$pt.count.ss <- renderValueBox({
        valueBox(achd.filtered() %>% nrow(),
                 'selected patients', 
                 width = 3, color = 'light-blue')
    })
    output$simple.count.ss <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 1) %>% nrow(), 
                 "Patients with Simple CHD", 
                 color = 'light-blue')
    })
    output$moderate.count.ss <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 2) %>% nrow(), 
                 "Patients with Moderate CHD", 
                 color = 'light-blue')
    })
    output$complex.count.ss <- renderValueBox({
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
            scale_x_discrete(name = "",
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
    
    ############################# LOCATIONS TAB #################################
     
    #---------------Value Boxes-------------------#
    # Value boxes with the key data points
    output$pt.count.loc <- renderValueBox({
        valueBox(achd.filtered() %>% nrow(),
                 'selected patients', 
                 width = 3, color = 'light-blue')
    })
    output$simple.count.loc <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 1) %>% nrow(), 
                 "Patients with Simple CHD", 
                 color = 'light-blue')
    })
    output$moderate.count.loc <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 2) %>% nrow(), 
                 "Patients with Moderate CHD", 
                 color = 'light-blue')
    })
    output$complex.count.loc <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 3) %>% nrow(), 
                 "Patients with Complex CHD", 
                 color = 'light-blue')
    })
    
    #---------------Leaflet-------------------#
    
    #Patient Locations Maps
    
    # create the box for the map, when the location update button is clicked
    observeEvent(input$loc.update, {
        output$locations.map.box <- renderUI({
            box(leafletOutput('locations.map', height = 550),
                width = 7, height = 580)
        })
    })
    # create the map withiin the box
    output$locations.map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = set.polys(),
                layerId = ~CODE16,
                fillColor = ~pal.loc()(ACHD_count),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels.loc(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addLegend(pal = pal.loc(), 
                      values = bins.loc(), 
                      opacity = 0.7, 
                      title = "ACHD population",
                      position = "bottomright")
    })
    
    # Create the summary box when the location update is clicked
    observeEvent(input$loc.update, {
        output$locations.summary.box <- renderUI({
            box(title = "Click an area on the map for detail information",
                width = 5, height = 580
            )
        })
    })
    
    # Print out area level information in summary box when the relevant area is clicked
    observeEvent( input$locations.map_shape_click, { 
        #---------------DATA SETUP-----------------#
        # the area click event
        event <- input$locations.map_shape_click
        # select the correct area
        event_area <- set.polys()@data %>% filter(CODE16 == event$id)
        # filter ACHD data for area only
        achd.area <- achd.filtered() %>% filter()
        
        #--------------CREATE BOX------------------#
        output$locations.summary.box <- renderUI({
            box(title = as.character(event_area[["NAME16"]]),
                htmlOutput("area.summary"),
                width = 5, height = 580
            )
        })
        
        #---------------BOX CONTENT----------------#
        output$area.summary <- renderUI({
            str1 <- paste("<strong>Number of ACHD patients:</strong> ", event_area[["ACHD_count"]])
            str2 <- paste("<strong>Number of patients lost to follow up:</strong> ", event_area[["ACHD_count"]])
            HTML(paste(str1, str2, sep = '<br/>'))
            
            })
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
