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
pt_data <- '/Users/calumnicholson/script/r-projects/achd-data/'

# Path to the study's data folder (note: only accessible at RPAH)
app_data <- './data/'

################################## LOAD DATA ############################################

#import ACHD data
achd <- readRDS(file = paste(pt_data, 'output/rpah_analysis_dataset.rds', sep=""))

#import DX coding
dx_codes <- read.csv(file = paste(app_data, '/ACHD_EPCC_coding.csv', sep=""), fileEncoding="UTF-8-BOM") %>% 
            filter(!(Variable == 'adm_AbsentPA' | Variable == 'PA' | Variable == "achd_id")) %>%
            select(!c(ACHD.Database.name, check., Variable))

#import Census data
sa2.TB <- readRDS(file = paste(app_data, 'AP_output/sa2_table_builder.rds', sep="")) %>%
                            mutate(IRSD = ordered(IRSD, c(1,2,3,4,5,6,7,8,9,10)),
                                   IRSAD = ordered(IRSAD, c(1,2,3,4,5,6,7,8,9,10)))

#import htt data
htt.nsw <- readRDS(file = paste(app_data, 'AP_output/htt_nsw.rds', sep=""))
htt.details <- readRDS(file = paste(app_data, 'AP_output/htt_details.rds', sep=""))

#current achd clinics
achd_ids <- c(646, 755, 152, 683, 737, 979)
#placeholer for adding new clinics
new_achd_ids <- achd_ids

#Import area polygons
sa2.polys <- readOGR(paste(app_data, 'shape_files/sa2_polys.shp', sep=""))
#sa3.polys <- readOGR(paste(app_data, 'shape_files/sa3_polys.shp', sep=""))
#sa4.polys <- readOGR(paste(app_data, 'shape_files/sa4_polys.shp', sep=""))

############################# HEADER CONTENT #######################################

header <- dashboardHeader(title = "ACHD in NSW")

############################# SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar(
    sidebarMenu(
      # Driving tab
        menuItem("Driving Time to Clinics", tabName = "driving", icon = icon("th")),
        # Snapshot tab
        menuItem("Snapshot of ACHD", tabName = "snapshot", icon = icon("dashboard")),
        # Location tab
        menuItem("Location of Patients", tabName = "locations", icon = icon("th")),
        
        
        # Global Filters
        h4("Global Filters"),
        div(style="display:inline-block",actionButton("sb.update", "Apply")),
        div(style="display:inline-block",actionButton("sb.reset", "Reset")),
        uiOutput("out.bethesda"),
        uiOutput("out.sex"),
        uiOutput("out.mortality"),
        uiOutput("out.age"),
        uiOutput("out.dates"),
        uiOutput("out.last.clinic")
    )
)

############################# BODY CONTENT #######################################


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
                # Summary Values
                fluidRow(valueBoxOutput('pt.count.loc', width = 3),
                         valueBoxOutput('simple.count.loc', width = 3),
                         valueBoxOutput('moderate.count.loc', width = 3),
                         valueBoxOutput('complex.count.loc', width = 3)
                ),
                fluidRow(
                        box(title = "Map Customisation",
                        selectInput('loc.poly.select', 'Select Area Level:',
                                    choices = c('SA2' = 2,
                                                'SA3' = 3,
                                                'SA4' = 4),
                                    selected = 2),
                        checkboxGroupInput("loc.gcc", "Filter by Region",
                                           choices = c('Greater Sydney' = "Greater Sydney",
                                                       'Rest of NSW' = "Rest of NSW",
                                                       'ACT' = "Australian Capital Territory"),
                                           selected = c("Greater Sydney", "Rest of NSW", "Australian Capital Territory")),
                        actionButton("loc.update", "Update"),
                        width = 3, height = 580
                        ),
                        uiOutput('locations.map.box'),
                         
                ),
                fluidRow(uiOutput('locations.summary.box')
                ),
                fluidRow(uiOutput('area.dx.box')
                ),
                fluidRow(uiOutput('area.irsd.box'),
                         uiOutput('area.aria.box'))
        ),
    
        #---------------Driving Tab-------------------#
        tabItem(tabName = "driving",
                #Title bar
                fluidRow(
                    valueBox("Driving time to ACHD Clinics",
                             "Where are the ACHD clinics in NSW and how long do patients have to drive to reach the neareset clinic?",
                             width = 12,
                             color = 'olive')
                ),

                # Summary Values
                fluidRow(valueBoxOutput('pt.count.drive', width = 3),
                         valueBoxOutput('simple.count.drive', width = 3),
                         valueBoxOutput('moderate.count.drive', width = 3),
                         valueBoxOutput('complex.count.drive', width = 3)
                ),
                fluidRow(
                    box(title = "Map Customisation",
                        checkboxGroupInput("drive.gcc", "Filter by Region",
                                           choices = c('Greater Sydney' = "Greater Sydney",
                                                       'Rest of NSW' = "Rest of NSW",
                                                       'ACT' = "Australian Capital Territory"),
                                           selected = c("Greater Sydney", "Rest of NSW", "Australian Capital Territory")),
                        selectInput("select.overlay", "Select area overlay:",
                                    choices = c("Driving Time to Nearest Clinic" = "drive.overlay",
                                                "Total ACHD population" = "achd.overlay"),
                                    selected = c("Driving Time to Nearest Clinic" = "drive.overlay")),
                        actionButton("drive.update", "Update"),
                        width = 4, height = 340
                    ),
                    box(uiOutput('phn.selector'),
                        uiOutput('lhn.selector'),
                        uiOutput('hospital.selector'),
                        uiOutput('clinic.button.add'),
                        uiOutput('clinic.button.reset'),
                        width = 4, height = 340
                    ),
                    box(title = "New Clinics",
                        uiOutput('new.clinics.output'),
                        width = 4, height = 340),
                ),
                fluidRow(box(leafletOutput('drive.map', height = 550),
                             width = 9, height = 580),
                         column(width = 3,
                             fluidRow(valueBoxOutput('dr.area.name', width = 12)),
                             fluidRow(valueBoxOutput('pt.count.area', width = 12)),
                             fluidRow(valueBoxOutput('pt.beth.area', width = 12)),
                             fluidRow(valueBoxOutput('pt.lft.area', width = 12)),
                             fluidRow(valueBoxOutput('pt.irsd.area', width = 12)),
                             fluidRow(valueBoxOutput('pt.remote.area', width = 12)),
                         )
                )
        )
    )
)

# Define UI for application
ui <- dashboardPage(header, sidebar, body)


############################# SERVER #################################

# Define server logic
server <- function(input, output) {
    
    # Function to search for input names
    getInputs <- function(pattern){
        reactives <- names(reactiveValuesToList(input))
        reactives[grep(pattern,reactives)]
    }
    
############################# REACTIVE FUNCTIONS #################################
    
    # Global Filters
    achd.filtered <- eventReactive(input$sb.update, {
        achd %>% 
            filter(death %in% input$sb.mortality) %>%
            filter(sex %in% input$sb.sex) %>%
            filter(bethesda_code %in% input$sb.bethesda) %>%
            filter(as.numeric(age, 'years') >= input$sb.age[1]) %>%
            filter(as.numeric(age, 'years') <= input$sb.age[2]) %>%
            filter(as.numeric(gap_2000, 'years') >= input$sb.last.clinic[1]) %>%
            filter(as.numeric(gap_2000, 'years') <= input$sb.last.clinic[2]) %>%
            {if ( !(input$sb.dates[1] == "2000-01-01" & input$sb.dates[2] == "2020-12-31") )
                mutate(., 
                       clinics_2000 = map(clinics_2000, ~ .x %>%
                                                 mutate(clinic_in_period = 
                                                        sapply(.$clinic_date, function(x) x %within% (as.Date(input$sb.dates[1])
                                                                                                      %--% 
                                                                                                      as.Date(input$sb.dates[2]))))),
                       in_time_period = map_dbl(map(clinics_2000, ~ .$clinic_in_period), any)) %>% 
                    filter(in_time_period == TRUE)
                else . }
    }) 
    
    # count frequency of diagnoses
    dx.count <- reactive({
        
        # created a dataset with only the disagnoses
        dx.df <- achd.filtered() %>%
            select(dx_codes$EPCC) %>%
            mutate_all(as.character) %>%
            mutate_all(as.numeric) %>%
            summarise_all(sum, na.rm = TRUE) %>%
            t() %>% as.data.frame() %>% 
            rename("dx_count" = V1) %>% 
            mutate(dx_label = dx_codes$Label[match(row.names(.), dx_codes$EPCC)])
        
        dx.df
    })
    
    # ------------------------- area-level data for locations map ---------------------------- #
    loc.area.achd <- eventReactive( input$loc.update, {
        if (input$loc.poly.select == 2) {
            achd.df <- achd.filtered() %>% rename("area" = sa2)
            TB.df <- sa2.TB %>% rename("area" = sa2_area)
        } else if (input$loc.poly.select == 3) {
            achd.df <- achd.filtered() %>% rename("area" = sa3)
        } else {
            achd.df <- achd.filtered() %>% rename("area"= sa4)}
        
        area.achd <- achd.df %>%
            group_by(area) %>% 
            summarise(ACHD_count = n(), 
                      beth_1 = sum(bethesda_code == 1),
                      beth_2 = sum(bethesda_code == 2), 
                      beth_3 = sum(bethesda_code == 3), 
                      beth_4 = sum(bethesda_code == 4))
        
        area.dx <- achd.df %>%
            mutate_at(as.character(dx_codes[['EPCC']]), as.character) %>% 
            mutate_at(as.character(dx_codes[['EPCC']]), as.numeric) %>%
            group_by(area) %>% 
            dplyr::summarise_at(as.character(dx_codes[['EPCC']]), sum, na.rm = TRUE)
        
        area.data <- left_join(TB.df, area.achd, by = 'area') %>% 
                        left_join(area.dx, by = 'area') %>%
                        mutate_at(as.character(dx_codes[['EPCC']]), 
                                  function(x) replace(x, is.na(x), 0)) %>%
                        mutate_at(c('ACHD_count', 'beth_1', 'beth_2', 'beth_3', 'beth_4'), 
                                  function(x) replace(x, is.na(x), 0))
        
        area.data
    })
    
    # ---------------------- plotting functions for locations map ------------------------------------ # 
    #set polygons for locations map
    loc.polys <- eventReactive( input$loc.update, {
        # select the correct shapefile
        if (input$loc.poly.select == 2) {polys <- sa2.polys} 
        else if (input$loc.poly.select == 3) {polys <- sa3.polys} 
        else {polys <- sa4.polys}
        
        # add the achd population for each area 
        polys <- merge(polys, loc.area.achd(), by.x = 'NAME16', by.y = 'area')
        #convert NAs to 0
        #polys@data <- polys@data %>% replace_na(list('ACHD_count' = 0))
        
        # Filter by the GCC areas selected
        polys.filtered <- subset(polys, polys$GCC_NAME16 %in% input$loc.gcc)
        polys.filtered
    })
    
    #Set bins for locations map
    bins.loc <- reactive({
        bins <- seq(0, 
                    plyr::round_any(max(loc.polys()$ACHD_count), 10, ceiling), 
                    plyr::round_any(max(loc.polys()$ACHD_count), 10, ceiling)/10)
        bins
    })
    
    #Set colour palette for locations map
    pal.loc <- reactive({
        pal <- colorBin("YlOrRd", domain = loc.polys()$ACHD_count, bins = bins.loc())
        pal
    })
    
    #Set labels for locations map
    labels.loc <- reactive({
        sprintf("<strong>%s</strong><br/>%g ACHD patients in area",
                 loc.polys()$NAME16, loc.polys()$ACHD_count) %>% lapply(htmltools::HTML)
    })

    #----------------- sa2 data for driving map --------------------------------------#
    
    drive.area.achd <- reactive({
    # Diagnosis severity counts in each area
    beth.drive <- achd.filtered() %>%
        group_by(sa2) %>%
        dplyr::summarise(ACHD_count = n(), 
                         beth_1 = sum(bethesda_code == 1),
                         beth_2 = sum(bethesda_code == 2), 
                         beth_3 = sum(bethesda_code == 3), 
                         beth_4 = sum(bethesda_code == 4),
                         ltf_3 = sum(ltf_3),
                         ltf_4 = sum(ltf_4),
                         ltf_5 = sum(ltf_5))
    
    # Diagnoses present in each area 
    dx.drive <- achd.filtered() %>%
        mutate_at(as.character(dx_codes[['EPCC']]), as.character) %>% 
        mutate_at(as.character(dx_codes[['EPCC']]), as.numeric) %>%
        group_by(sa2) %>% 
        dplyr::summarise_at(as.character(dx_codes[['EPCC']]), sum, na.rm = TRUE)
    
    # Join above to table builer data
    area.drive <- left_join(sa2.TB, beth.drive, by = c('sa2_area' = 'sa2')) %>% 
        left_join(dx.drive, by = c('sa2_area' = 'sa2')) %>%
        mutate_at(as.character(dx_codes[['EPCC']]), function(x) replace(x, is.na(x), 0)) %>%
        mutate_at(c('ACHD_count', 'beth_1', 'beth_2', 'beth_3', 'beth_4',
                    'ltf_3', 'ltf_4', 'ltf_5'), function(x) replace(x, is.na(x), 0))
    
    area.drive
    })
    

    
############################# SIDEBAR #################################
    
    output$out.bethesda <- renderUI ({
        checkboxGroupInput("sb.bethesda", "Disease severity",
                           choices = c('Simple' = 1,
                                       'Moderate' = 2,
                                       'Complex' = 3,
                                       'Unknown' = 4),
                           selected = c(1, 2, 3))
    })
    
    output$out.sex <- renderUI ({
        checkboxGroupInput("sb.sex", "Sex",
                           choices = c('Male' = 1,
                                       'Female' = 2,
                                       'Neither' = 4),
                           selected = c(1, 2, 4))
    })
      
    output$out.mortality <- renderUI ({
        checkboxGroupInput("sb.mortality", "Deceased patients",
                           choices = c('Alive' = 0,
                                       'Deceased' = 1),
                           selected = c(0))
    })
    
    output$out.age <- renderUI ({
        sliderInput('sb.age', 'Age',
                    min = 18,
                    max = 110,
                    value = c(18, 110),
                    round = TRUE)
    })
    
    output$out.dates <- renderUI ({
        dateRangeInput("sb.dates", "Select a time period:",
                       start = "2000-01-01", end = "2020-12-31",
                       min = "2000-01-01", max = "2020-12-31",
                       format = "dd/mm/yyyy")
    })
       
    output$out.last.clinic <- renderUI ({ 
        sliderInput('sb.last.clinic', 'Time since last clinic visit',
                    min = 0,
                    max = 21,
                    value = c(0,21),
                    round = TRUE)
    })
        
    
    observeEvent(input$sb.reset, {
    
        output$out.bethesda <- renderUI ({
            checkboxGroupInput("sb.bethesda", "Disease severity",
                               choices = c('Simple' = 1,
                                           'Moderate' = 2,
                                           'Complex' = 3,
                                           'Unknown' = 4),
                               selected = c(1, 2, 3))
        })
        
        output$out.sex <- renderUI ({
            checkboxGroupInput("sb.sex", "Sex",
                               choices = c('Male' = 1,
                                           'Female' = 2,
                                           'Neither' = 4),
                               selected = c(1, 2, 4))
        })
        
        output$out.mortality <- renderUI ({
            checkboxGroupInput("sb.mortality", "Deceased patients",
                               choices = c('Alive' = 0,
                                           'Deceased' = 1),
                               selected = c(0))
        })
        
        output$out.age <- renderUI ({
            sliderInput('sb.age', 'Age',
                        min = 18,
                        max = 110,
                        value = c(18, 110),
                        round = TRUE)
        })
        
        output$out.dates <- renderUI ({
            dateRangeInput("sb.dates", "Select a time period:",
                           start = "2000-01-01", end = "2020-12-31",
                           min = "2000-01-01", max = "2020-12-31",
                           format = "dd/mm/yyyy")
        })
        
        output$out.last.clinic<- renderUI ({ 
            sliderInput('sb.last.clinic', 'Time since last clinic visit',
                        min = 0,
                        max = 21,
                        value = c(0,21),
                        round = TRUE)
        })
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
        ggplot(achd.filtered(), aes(x=no_clinics_2000)) +
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
        dx.count() %>% filter(dx_count > 0) %>%
        ggplot(aes(x = dx_label, y=dx_count)) + 
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
                width = 9, height = 580)
        })
    })
    # create the map withiin the box
    output$locations.map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = loc.polys(),
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
        event_area <- loc.area.achd() %>% filter(SA2_5DIGIT == event$id)
        
        #--------------CREATE BOX------------------#
        output$locations.summary.box <- renderUI({
            box(title = as.character(event_area[["area"]]),
                htmlOutput("area.summary"),
                width = 12, height = 200
            )
        })
        
        #---------------BOX CONTENT----------------#
        output$area.summary <- renderUI({
            str_ptno <- paste("<strong>Number of ACHD patients:</strong> ", 
                              event_area[["ACHD_count"]], 
                              sep = "")
            str_drive <- paste("<strong>Driving time to nearest clinic:</strong> ", 
                          as.period(event_area[["shortest_time"]])@hour, "hrs ",
                          as.period(event_area[["shortest_time"]])@minute, "min", 
                          sep = "")
            str_bethesda <- paste("<strong>Simple | Moderate | Severe -</strong> ",
                                  event_area[["beth_1"]], " | ",
                                  event_area[["beth_2"]], " | ",
                                  event_area[["beth_3"]], " | ",
                                  sep = "")
            HTML(paste(str_ptno, str_drive, str_bethesda, sep = '<br/>'))
            
            })
        
        output$area.dx.box <- renderUI({
            box(title = "CHD Diagnoses in ",
                plotOutput("dx_area_plot"),
                width = 12, height = 580
                )
            })
            
            output$dx_area_plot <- renderPlot({
                    event_area %>% select(dx_codes$EPCC) %>% 
                                   t() %>% as.data.frame() %>%
                                   rename("dx_count" = V1) %>% 
                                   mutate(dx_label = dx_codes$Label[match(row.names(.), dx_codes$EPCC)]) %>%
                                   filter(dx_count > 0) %>%
                    ggplot(aes(x = dx_label, y=dx_count)) + 
                    geom_bar(stat="identity") +
                    theme(axis.text.x = element_text(angle = 90)) +
                    labs(title = "Frequecy of each diagnosis", 
                         y = "count",
                         x = "") +
                    coord_flip()
            })
        output$area.irsd.box <- renderUI({
            box(title = "Disadvantage",
                plotOutput("irsd_area_plot"),
                width = 6, height = 580)
            })
        output$irsd_area_plot <- renderPlot({
            irsd.data <- loc.area.achd() %>% select(area, SA2_5DIGIT, IRSD) 
            
            irsd.data$cuts <- cut(as.numeric(irsd.data$IRSD), breaks = 30, label = F)
            coloured_cut <- irsd.data$cuts[irsd.data$SA2_5DIGIT == event$id]
            irsd.data$color <- ifelse(irsd.data$cuts == coloured_cut[1], "Selected", "")
            
            irsd.data %>% filter(!is.na(IRSD)) %>%
                ggplot(aes(x=IRSD, fill = cuts == coloured_cut[1])) +
                geom_bar() +
                scale_fill_manual(values = c("grey45", "red"))+
                theme_minimal() + theme(legend.position = "none")
            })
        
        output$area.aria.box <- renderUI ({
            box(title = "Remoteness",
                plotOutput('aria_area_plot'),
                width = 6, height = 580)
            })
        output$aria_area_plot <- renderPlot ({
            aria.long <- event_area %>% 
                select(area, SA2_5DIGIT, major_cities:very_remote) %>%
                pivot_longer(cols = major_cities:very_remote, names_to = "remoteness")
            
            aria.long$remoteness <- factor(aria.long$remoteness, levels = aria.long$remoteness)
            
            ggplot(data = aria.long, aes(x = factor(remoteness), y = value)) +
                geom_col() +
                scale_y_continuous(limits = c(0,1), labels = percent) +
                theme_minimal()
            
        })
        
        
    })
    
############################# DRIVING TAB #################################
    
    #---------------Value Boxes-------------------#
    # Value boxes with the key data points
    output$pt.count.drive <- renderValueBox({
        valueBox(achd.filtered() %>% nrow(),
                 'selected patients', 
                 width = 3, color = 'light-blue')
    })
    output$simple.count.drive <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 1) %>% nrow(), 
                 "Patients with Simple CHD", 
                 color = 'light-blue')
    })
    output$moderate.count.drive <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 2) %>% nrow(), 
                 "Patients with Moderate CHD", 
                 color = 'light-blue')
    })
    output$complex.count.drive <- renderValueBox({
        valueBox(achd.filtered() %>% filter(bethesda_code == 3) %>% nrow(), 
                 "Patients with Complex CHD", 
                 color = 'light-blue')
    })
    
    #------------------Map Options----------------#
    # this phn selector
    output$phn.selector <- renderUI({
        selectizeInput('phn.type', 'Select Primary Health Network Area:',
                    choices = unique(htt.details$Primary.Health.Network.area..PHN.),
                    options = list(
                        placeholder = 'Please select an option below',
                        onInitialize = I('function() { this.setValue(""); }'))
        )
    })
    
    # The lhn selector
    observeEvent(input$phn.type, {
        
        if (input$phn.type != "") {
        
        output$lhn.selector <- renderUI({
            
            available.lhn <- htt.details %>% filter(Primary.Health.Network.area..PHN. %in% input$phn.type)
            
            selectizeInput('lhn.type', 'Select Local Hospital Network Area:',
                        choices = unique(available.lhn$Local.Hospital.Network..LHN.),
                        options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }'))
            )
        })
        
        }
    }, ignoreInit = TRUE)
        
    # The hospital selector
    observeEvent(input$lhn.type, {
        
        if (input$lhn.type != "") {
        
        output$hospital.selector <- renderUI({
            
            
            available.hosp <- htt.details %>% filter(Primary.Health.Network.area..PHN. %in% input$phn.type) %>%
                                              filter(Local.Hospital.Network..LHN. %in% input$lhn.type)
            
            selectizeInput('hospital', 'Select Hospital:',
                        choices = unique(available.hosp$Hospital.name),
                        options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }'))
            )
        })
        
        }
    }, ignoreInit = TRUE)
    
    # Add and Reset Clinics buttons
    observeEvent(input$hospital, {
        
        if (input$hospital != "") {
            
            output$clinic.button.add <-  renderUI({
                actionButton("drive.add", "Add Clinic")
            })
            
            output$clinic.button.reset <-  renderUI({
                actionButton("drive.clinic.reset", "Reset Clinics")
            })
            
        }
    })
    
    # Markers for hospital locations
    # Current ACHD clinics and new clinics will appear different colours
    HospitalIcons <- awesomeIconList(
        Current = makeAwesomeIcon(
            icon = 'hospital-o', 
            markerColor = 'green', 
            iconColor = 'white', 
            library = "fa"), 
        New = makeAwesomeIcon(
            icon = 'hospital-o', 
            markerColor = 'orange', 
            iconColor = 'white', 
            library = "fa") )
    
    #-------------Reactive Values for Driving Map--------------------#
    drive.values <- reactiveValues()
    #For creating and editing the clinics table
    drive.values$add_clinic_table <- data.frame(Hospital = as.character(), 
                                            ID = as.character(), 
                                            stringsAsFactors = FALSE)
    #For tracking the ACHD clinic IDs selected
    drive.values$new_achd_ids <- c(646, 755, 152, 683, 737, 979)
    #For the area level data
    observe({ drive.values$area_data <- drive.area.achd() })
    
    # When new clinics are added ('Add Clinic' Button is clicked), do the following
    observeEvent(input$drive.add, {
        
        # The hospital ID that was selected
        hospital_id <- htt.details$Hospital_ID[htt.details$Hospital.name == input$hospital]
        
        # New row to add to clinic table
        new.row <- isolate(data.frame(Hospital = input$hospital, ID = hospital_id, stringsAsFactors = FALSE))
        
        # Add the new row to the clinic table, if it wasnt already selected
        if ( !(hospital_id %in% isolate(drive.values$add_clinic_table$ID)) )
        { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row)) }
        
        # Display the table
        output$new.clinics.output <- renderUI({
            output$new.clinics.table <- renderTable(drive.values$add_clinic_table)
            tableOutput("new.clinics.table")
        })
        
        # Add the new hospital id to the clinics id list
        if ( !(hospital_id %in% isolate(drive.values$new_achd_ids)) ) 
        { isolate(drive.values$new_achd_ids <- append(drive.values$new_achd_ids, hospital_id)) }
        
        # Use the new clinics id list to recalculate the driving time to the nearest hospital
        shortest_time_new <- htt.nsw %>%
            select(SA2_5DIGIT, as.character(drive.values$new_achd_ids)) %>%
            mutate(shortest_time = pmap_dbl(
                .l = select(., -SA2_5DIGIT),
                .f = function(...) min(...)),
                shortest_time = duration(shortest_time, "seconds")) %>%
            select(SA2_5DIGIT, shortest_time)
        
        # Add the new shortest dirving time column into the area data
        drive.values$area_data <- drive.values$area_data %>%
            select(-shortest_time) %>%
            left_join(shortest_time_new, by = "SA2_5DIGIT")
    })
    
    # Reset the ACHD clinics list ('Reset Clinics' Button is clicked)
    observeEvent(input$drive.clinic.reset, {

        # Clear the clinics table
        drive.values$add_clinic_table <- NULL
        
        # Display the table as nothing
        output$new.clinics.output <- renderUI({
            output$new.clinics.table <- renderTable(drive.values$add_clinic_table)
            tableOutput("new.clinics.table")
        })
        
        # Reset the achd clinic ids to the original clinics
        drive.values$new_achd_ids <- c(646, 755, 152, 683, 737, 979)
        
        # Reset the area data
        drive.values$area_data <- drive.area.achd()
    })
    
    ############# Reactive Functions for Mapping ###############
    
    # -------------- sa2 polys for driving map ----------------------------------- #
    drive.polys <- eventReactive(input$drive.update, {
        #add area data to polygons
        drive.poly <- merge(sa2.polys, drive.values$area_data, by.x = 'NAME16', by.y = 'sa2_area')
        
        # Filter by the GCC areas selected
        polys.filtered <- subset(drive.poly, drive.poly$GCC_NAME16 %in% input$drive.gcc)
        polys.filtered
    })
    
    #set state centre coords function
    set_coords <- reactive({
        coords <- do.call(rbind,lapply(drive.polys()@polygons, function(x) 
        {data.frame(long = c(min(x@Polygons[[1]]@coords[,1]),
                             max(x@Polygons[[1]]@coords[,1])),
                    lat = c(min(x@Polygons[[1]]@coords[,2]),
                            max(x@Polygons[[1]]@coords[,2]))
        )
        })
        )
        coords
    })
    
    # Set bins for driving map
    bins.drive <- eventReactive(input$drive.update, {
        bins <- seq(0, 
                    plyr::round_any(max(as.numeric(drive.polys()$shortest_time, 'hours'), na.rm = TRUE), 10, ceiling), 
                    plyr::round_any(max(as.numeric(drive.polys()$shortest_time, 'hours'), na.rm = TRUE), 10, ceiling)/10)
        bins
    })
    
    # Set colour palette for driving map
    pal.drive <- eventReactive(input$drive.update, {
        pal <- colorNumeric("YlOrRd", domain = as.numeric(drive.polys()$shortest_time, 'hours'))
        pal
    })
    
    #Set bins for locations map
    bins.loc2 <- reactive({
        bins <- seq(0, 
                    plyr::round_any(max(drive.polys()$ACHD_count), 10, ceiling), 
                    plyr::round_any(max(drive.polys()$ACHD_count), 10, ceiling)/10)
        bins
    })
    
    #Set colour palette for locations map
    pal.loc2 <- reactive({
        pal <- colorNumeric("YlOrRd", domain = drive.polys()$ACHD_count)
        pal
    })
    
    # Set labels for driving map
    labels.drive <- eventReactive(input$drive.update, {
        sprintf(
            "<strong>%s</strong><br/>
            %i ACHD patients<br/>
            Simple: %i | Moderate: %i | Severe: %i<br/>
            %.1f hr drive to nearest clinic<br/>",
            drive.polys()$NAME16, 
            drive.polys()$ACHD_count,
            drive.polys()$beth_1, drive.polys()$beth_2, drive.polys()$beth_3,
            as.numeric(drive.polys()$shortest_time, 'hours')
            ) %>% 
                lapply(htmltools::HTML)
    })
    
    #------------------Base Map----------------#
    # output the basic version of the map with elements that don't change
    output$drive.map <- renderLeaflet({ 
        leaflet() %>%
            addTiles() %>%
            setView(147.016667, -32.163333, zoom = 5.5) %>%
            addAwesomeMarkers(
                data = htt.details %>% filter(Hospital_ID %in% achd_ids),
                lng = ~Longitude,
                lat = ~Latitude,
                label = ~Hospital.name,
                icon = ~HospitalIcons["Current"])  
    })
    
    ############# Observers for Map editting ###############
    
    # Adding polygons
    observeEvent(input$drive.update, {
        
        if (input$select.overlay == 'drive.overlay') {
        
        leafletProxy("drive.map") %>% 
            # Clear old polygons
            clearShapes %>%
            # Add new polygons
            addPolygons(
                data = drive.polys(),
                layerId = ~CODE16,
                fillColor = ~pal.drive()(as.numeric(drive.polys()$shortest_time, 'hours')),
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
                label = labels.drive(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                fitBounds(min(set_coords()$long),
                          min(set_coords()$lat),
                          max(set_coords()$long),
                          max(set_coords()$lat))
            
        } else if (input$select.overlay == 'achd.overlay') {
            
            leafletProxy("drive.map") %>% 
                # Clear old polygons
                clearShapes %>%
                addPolygons(
                    data = drive.polys(),
                    layerId = ~CODE16,
                    fillColor = ~pal.loc2()(ACHD_count),
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
                    label = labels.drive(),
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
                fitBounds(min(set_coords()$long),
                          min(set_coords()$lat),
                          max(set_coords()$long),
                          max(set_coords()$lat))
        }
    })
    
    # Adding a Legend
    observeEvent(input$drive.update, {
        
        if (input$select.overlay == 'drive.overlay') {
            
            data <- drive.polys()@data %>% filter(!is.na(shortest_time))
            
            leafletProxy("drive.map") %>%
                # Clear old legend 
                clearControls() %>%
                # Add Legend
                addLegend(pal = pal.drive(), 
                          values = as.numeric(data$shortest_time, 'hours'), 
                          opacity = 0.7, 
                          title = "Driving time (hrs)",
                          position = "bottomright",
                          layerId = 'drive.legend')
        
        } else if (input$select.overlay == 'achd.overlay') {
        
            leafletProxy("drive.map") %>%
                # Clear old legend 
                clearControls() %>%
                # Add Legend
                addLegend(pal = pal.loc2(), 
                          values = drive.polys()$ACHD_count, 
                          opacity = 0.7, 
                          title = "ACHD population",
                          position = "bottomright")
        }
            
    })
    
    # Adding Markers for New clinics
    observeEvent(input$drive.update, {
        if ( length(drive.values$add_clinic_table$ID != 0)) {
            leafletProxy("drive.map") %>%
                # Clear old markers 
                clearGroup(group = "new_markers") %>%
                # Add markers for new clinics
                addAwesomeMarkers(
                    data = htt.details %>% filter(Hospital_ID %in% drive.values$add_clinic_table$ID),
                    lng = ~Longitude,
                    lat = ~Latitude,
                    label = ~Hospital.name,
                    group = "new_markers",
                    icon = ~HospitalIcons["New"]
                    
                )
        } else {
            leafletProxy("drive.map") %>%
                # Clear old markers 
                clearGroup(group = "new_markers")
        }
        
    })
    
    # Print out area level information in summary box when the relevant area is clicked
    observeEvent( input$drive.map_shape_click, { 
      #---------------DATA SETUP-----------------#
      # the area click event
      event <- input$drive.map_shape_click
      # select the correct area
      drive.values$event_area <- drive.values$area_data %>% filter(SA2_5DIGIT == event$id)
      
      #---------------AREA VALUE BOXES-----------#
      output$dr.area.name <- renderValueBox({
        valueBox(tags$p(drive.values$event_area$sa2_area, style = "font-size: 50%;"),
                 '', 
                 width = 12, color = 'light-blue')
      })
      
      output$pt.count.area <- renderValueBox({
        valueBox(drive.values$event_area$ACHD_count,
                 'ACHD patients', 
                 width = 12, color = 'light-blue')
      })
      
      output$pt.beth.area <- renderValueBox({
        valueBox(paste(drive.values$event_area$beth_1, " | ",
                       drive.values$event_area$beth_2, " | ",
                       drive.values$event_area$beth_3, 
                       sep = "  "),
                 'Simple | Moderate | Severe', 
                 width = 12, color = 'light-blue')
      })
      
      output$pt.lft.area <- renderValueBox({
        valueBox(drive.values$event_area$ltf_3,
                 'Lost to Follow Up', 
                 width = 12, color = 'light-blue')
      })
      
      output$pt.irsd.area <- renderValueBox({
        valueBox(drive.values$event_area$IRSD,
                 'Level of Disadvantage', 
                 width = 12, color = 'light-blue')
      })
      
      output$pt.remote.area <- renderValueBox({
        valueBox(tags$p(
                    paste(percent(drive.values$event_area$major_cities),
                       " | ",
                       percent(drive.values$event_area$inner_regional + drive.values$event_area$outer_regional), 
                       " | ",
                       percent(drive.values$event_area$remote + drive.values$event_area$very_remote), 
                       sep = "  "),
                    style = "font-size: 75%;"),
                 'Cities | Regional | Remote', 
                 width = 12, color = 'light-blue')
      })
      
      
      
      
    })
    
#This is the closing braket for the server        
}


# Run the application 
shinyApp(ui = ui, server = server)
