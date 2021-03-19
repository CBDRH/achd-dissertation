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
library(scales)
library(lubridate)
library(ggplot2)
library(rgdal)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tableHTML)

# Path to ACHD database data (note: only accessible at RPAH)
pt_data <- '/Users/calumnicholson/script/r-projects/achd-data/'

# Path to the app data
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
achd_ids <- c(152, 408, 646, 683, 737, 979)
#placeholer for adding new clinics
new_achd_ids <- achd_ids

#Import area polygons
sa2.polys <- readOGR(paste(app_data, 'shape_files/sa2_polys.shp', sep=""))
#sa3.polys <- readOGR(paste(app_data, 'shape_files/sa3_polys.shp', sep=""))
#sa4.polys <- readOGR(paste(app_data, 'shape_files/sa4_polys.shp', sep=""))


############################# HEADER CONTENT #######################################

header <- dashboardHeader(title = "Clinic Planning Tool")


############################# SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Welcome Page
        menuItem("Welcome", tabName = "welcome"),
        # Driving tab
        menuItem("Clinic Planning", tabName = "driving"),
        # Snapshot tab
        menuItem("Snapshot of ACHD", tabName = "snapshot", icon = icon("dashboard")),
        
        # Report Download Button
        downloadButton("report.dl", "Download", icon=icon("download")),
        tags$head(tags$style(".dl_butt{color:blue;}")),
        
        # Global Filters
        h4("Global Filters"), # Title
        div(style="display:inline-block",actionButton("sb.update", "Apply")), # Apply Filters button
        div(style="display:inline-block",actionButton("sb.reset", "Reset")), # Reset Filters button
        uiOutput("out.bethesda"), # filter by disease severity
        uiOutput("out.sex"), # fitler by sex
        uiOutput("out.mortality"), # filter my mortality
        uiOutput("out.age"), # filter by age
        uiOutput("out.dates"), # filter by time range
        uiOutput("out.last.clinic") # filter by time since last clinic visit
    )
)


############################# BODY CONTENT #######################################
body <- dashboardBody(
    tabItems(
        #---------------Welcome Tab-------------------#
        tabItem(tabName = "welcome",
                fluidRow(
                  
                  # Title bar
                  valueBox("Clinic Planning in Adult Congenital Heart Disease",
                           "Combining geographic information systems, census data and patient information to determine
                           appropraite clinic locations in NSW",
                           width = 10,
                           color = 'olive'),
                  tags$head(tags$style(make_css(list('.btn', 'white-space', 'pre-wrap')))),
                  
                  # Button to load patient data
                  actionButton('load.data', 
                               HTML("Load Patient Data"),
                               style ="display:block;
                                       height: 102px;
                                       width: 175px;
                                       border: 2px solid black;
                                       font-size: 200%;" )
                ),
                
                # Introduction and background information
                fluidRow(
                  box(title = "Introduction",
                      
                    
                  ),
                  box(title = "Data Sources"
                      
                  ),
                ),
                fluidRow(
                  box(title = "Key Definitions"
                      
                  ),
                  box(title = "References"
                    
                    
                  )
                )
        ),
        #---------------Driving Tab-------------------#
        tabItem(tabName = "driving",
                # Instructions for using driving tab
                box(title = "Instructions", collapsible = T, width = 12),
                
                
                fluidRow(
                  
                  # Map Customisation
                  box(title = "Map Customisation",
                      # Filtering by Region
                      checkboxGroupInput("drive.gcc", "Filter by Region",
                                         choices = c('Greater Sydney' = "Greater Sydney",
                                                     'Rest of NSW' = "Rest of NSW",
                                                     'ACT' = "Australian Capital Territory"),
                                         selected = c("Greater Sydney", "Rest of NSW", "Australian Capital Territory")),
                      
                      # Select Map Overlay
                      selectInput("select.overlay", "Select area overlay:",
                                  choices = c("Driving Time to Nearest Clinic" = "drive.overlay",
                                              "Total ACHD population" = "achd.overlay"),
                                  selected = c("Driving Time to Nearest Clinic" = "drive.overlay")),
                      actionButton("drive.update", "Update"),
                      width = 4, height = 340
                  ),
                  
                  # Selecting New Clinics
                  box(title = "New Clinic Selection",
                      uiOutput('phn.selector'), # Primary Health Network
                      uiOutput('lhn.selector'), # Local Health Network
                      uiOutput('hospital.selector'), # Hospital
                      div(style="display:inline-block",uiOutput('clinic.button.add')), # Add hospital to list
                      div(style="display:inline-block",uiOutput('clinic.button.reset')), # Reset list
                      width = 4, height = 340
                  ),
                  
                  # Information about Hospital Coverage
                  tabBox(title = NULL,
                         id = "clinic.tabs", 
                         tabPanel("Current Clinics", uiOutput("current.clinic.output")), # Current hospitals
                         tabPanel("New Clinics", uiOutput("new.clinics.output")), # Newly Selected Hospitals
                         width = 4, height = 340),
                ),
                
                # Summary Value Boxes
                fluidRow(valueBoxOutput('pt.count.drive', width = 3), # Total Number of Patients
                         valueBoxOutput('beth.count.drive', width = 3), # Breakdown of disease Severity
                         valueBoxOutput('ltf.count.drive', width = 3), # Number of Patients lost to follow up
                         valueBoxOutput('hr.count.drive', width = 3) # Number of patients within a 1 hour drive
                ),
                
                # Map and Area level value boxes
                fluidRow(box(leafletOutput('drive.map', height = 550),
                             width = 9, height = 580),
                         column(width = 3,
                             fluidRow(valueBoxOutput('dr.area.name', width = 12)), # Area name
                             fluidRow(valueBoxOutput('pt.count.area', width = 12)), # Patient Count
                             fluidRow(valueBoxOutput('pt.beth.area', width = 12)), # Breakdown of disease Severity
                             fluidRow(valueBoxOutput('pt.lft.area', width = 12)), # Number of Patients lost to follow up
                             fluidRow(valueBoxOutput('pt.irsd.area', width = 12)), # Area disadvantage
                             fluidRow(valueBoxOutput('pt.remote.area', width = 12)), # Area Remoteness
                         )
                ),
                
                fluidRow(uiOutput('area.dx.box') # Diagnoses Present in selected area
                )
        ),
        
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
    achd.filtered <- eventReactive( 
      
        c(input$sb.update,
        input$load.data), ignoreInit = T, # Triggers on either load data, or update filters
        
        {achd %>% 
            filter(death %in% input$sb.mortality) %>% # Filter by mortality status
            filter(sex %in% input$sb.sex) %>% # filter by sex
            filter(bethesda_code %in% input$sb.bethesda) %>% # Filter by Disease Severity
            filter(as.numeric(age, 'years') >= input$sb.age[1]) %>% # Filter by Age; lower bound
            filter(as.numeric(age, 'years') <= input$sb.age[2]) %>% # Filter by Age; upper bound
            filter(as.numeric(gap_2000, 'years') >= input$sb.last.clinic[1]) %>% # Filter by last clinic date; lower bound
            filter(as.numeric(gap_2000, 'years') <= input$sb.last.clinic[2]) %>% # filter by last clinic date; upper bound
            
            # Filtering by the selected date range. This is quite a slow process and will only run is the date range 
            # is altered from the default, which is all-inclusive
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
    

############################# DRIVING TAB #################################
 
    #-------------Reactive Values for Driving Map--------------------#
    drive.values <- reactiveValues()
    #For creating and editing the clinics table
    drive.values$add_clinic_table <- data.frame(Hospital = as.character(), 
                                                Patients = as.character(),
                                                ltf = as.character(),
                                                stringsAsFactors = FALSE)
    #For tracking the ACHD clinic IDs selected
    drive.values$new_achd_ids <- c(646, 755, 152, 683, 737, 979)
    #For the area level data
    observe({ drive.values$area_data <- drive.area.achd() })
    
    #Create driving data table for the current clinics
    observe({
    current_hospitals <- htt.nsw %>%
                          select(SA2_5DIGIT, as.character(achd_ids)) %>%
                          mutate_at(as.character(achd_ids), as.duration) %>%
                          left_join(drive.area.achd(), by ='SA2_5DIGIT') %>%
                          select(as.character(achd_ids), sa2_area, SA2_5DIGIT, ACHD_count, 
                                 ltf_3, beth_1, beth_2, beth_3)
    })
    
    # Make a table summarise the current clinics
    drive.values$current_clinic_table <- data.frame(Hospital = htt.details %>% filter(Hospital_ID %in% achd_ids) %>% 
                                                      pull(Hospital.name),
                                                    Patients = sapply(achd_ids, function(x)
                                                      current_hospitals %>% 
                                                        filter(as.numeric(!!as.symbol(x), "hours") <= 1) %>%
                                                        summarise(hr_drive = sum(ACHD_count)) %>%
                                                        pull(hr_drive) %>% as.integer()
                                                    ),
                                                    ltf = sapply(achd_ids, function(x)
                                                      current_hospitals %>% 
                                                        filter(as.numeric(!!as.symbol(x), "hours") <= 1) %>%
                                                        summarise(hr_drive = sum(ltf_3)) %>%
                                                        pull(hr_drive) %>% as.integer()
                                                    ),
                                                    stringsAsFactors = FALSE)
    
    # Output the table
    output$current.clinic.output <- renderUI({
      output$current.clinics.table <- renderTable(drive.values$current_clinic_table)
      tableOutput("current.clinics.table")
    })
    
    # When new clinics are added ('Add Clinic' Button is clicked), do the following
    observeEvent(input$drive.add, {
      
      # The hospital ID that was selected
      hospital_id <- htt.details$Hospital_ID[htt.details$Hospital.name == input$hospital]
      
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
      
      # Get driving information for select hospital
      selected_hospital <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(hospital_id)) %>%
        mutate(hospital = as.duration(.[[as.character(hospital_id)]])) %>%
        left_join(drive.values$area_data, by ='SA2_5DIGIT') %>%
        select(hospital, sa2_area, SA2_5DIGIT, ACHD_count, ltf_3, beth_1, beth_2, beth_3)
      
      # New row to add to clinic table
      new.row <- isolate(data.frame(Hospital = input$hospital, 
                                    Patients = selected_hospital %>% filter(as.numeric(hospital, "hours") <= 1) %>%
                                                                     summarise(hr_drive = sum(ACHD_count)) %>%
                                                                     pull(hr_drive) %>% as.integer(), 
                                    ltf = selected_hospital %>% filter(as.numeric(hospital, "hours") <= 1) %>%
                                                                summarise(hr_ltf = sum(ltf_3)) %>%
                                                                pull(hr_ltf) %>% as.integer(),
                                    stringsAsFactors = FALSE))
      
      # Add the new row to the clinic table, if it wasnt already selected
      if ( !(hospital_id %in% isolate(drive.values$add_clinic_table$ID)) )
      { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row)) }
      
      # Display the new clinics table
      output$new.clinics.output <- renderUI({
        output$new.clinics.table <- renderTable(drive.values$add_clinic_table)
        tableOutput("new.clinics.table")
      })
      
      
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
    
    #---------------Value Boxes-------------------#
    # Value boxes with the key data points
    observeEvent(input$drive.update, {
    output$pt.count.drive <- renderValueBox({
        valueBox(tags$p(achd.filtered() %>% nrow(),
                        style = "font-size: 75%;"),
                 'selected patients', 
                 width = 3, color = 'light-blue')
    })
    output$beth.count.drive <- renderValueBox({
        valueBox(tags$p(
          paste(achd.filtered() %>% filter(bethesda_code == 1) %>% nrow(), " | ",
                achd.filtered() %>% filter(bethesda_code == 2) %>% nrow(), " | ",
                achd.filtered() %>% filter(bethesda_code == 3) %>% nrow(), 
                sep = "  "),
          style = "font-size: 75%;"),
          'Simple | Moderate | Severe',
           color = 'light-blue'
        )
    })
    output$ltf.count.drive <- renderValueBox({
        valueBox(tags$p(sum(achd.filtered()$ltf_3),
                        style = "font-size: 75%;"), 
                 "Patients Lost to Follow up", 
                 color = 'light-blue')
    })
    output$hr.count.drive <- renderValueBox({
      valueBox(tags$p(
                 drive.values$area_data %>% filter(as.numeric(shortest_time, "hours") <= 1) %>%
                                 summarise(hr_drive = sum(ACHD_count)) %>%
                                 pull(hr_drive),
                 style = "font-size: 75%;"),
                 "Patients in 1hr drive to ACHD clinic", 
                 color = 'light-blue')
    })
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
      
      #---------------AREA DIAGNOSES-----------#
      output$area.dx.box <- renderUI({
        box(title = paste("CHD Diagnoses in ", drive.values$event_area$sa2_area),
            plotOutput("dx_area_plot"),
            width = 12, height = 580
        )
      })
      
      output$dx_area_plot <- renderPlot({
        drive.values$event_area %>% select(dx_codes$EPCC) %>% 
          t() %>% as.data.frame() %>%
          rename("dx_count" = V1) %>% 
          mutate(dx_label = dx_codes$Label[match(row.names(.), dx_codes$EPCC)]) %>%
          filter(dx_count > 0) %>%
          ggplot(aes(x = reorder(dx_label, dx_count), y=dx_count)) + 
          geom_bar(stat="identity") +
          theme(axis.text.x = element_text(angle = 90)) +
          labs(title = "Frequecy of each diagnosis", 
               y = "count",
               x = "") +
          coord_flip()
      })
      
    })
    
#This is the closing braket for the server            
    
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
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
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
    
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
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
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
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
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
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
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
        output$dx_box <- renderUI({
            box(plotOutput("dx_plot", height = 750), 
                title = "Frequency of Each Diagnosis",
                width = 12, height = 825)
        })
    })
    output$dx_plot <- renderPlot({
        dx.count() %>% filter(dx_count > 0) %>% 
        ggplot(aes(x = reorder(dx_label, dx_count), y=dx_count)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90)) +
            labs(title = "Frequecy of each diagnosis", 
                 y = "count",
                 x = "") +
            coord_flip()
    })
    




    
############################# DOWNLOAD REPORT #################################
    output$report.dl <- downloadHandler(
      filename = "Report.html",
      
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReport <- file.path(tempdir(), "Report.Rmd")
        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
        
        rmarkdown::render(tempReport, output_file = file)
        
      }
      
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)
