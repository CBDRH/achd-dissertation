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
library(here)

# Path to ACHD database data (note: only accessible at RPAH)
pt_data <- '/Users/calumnicholson/script/r-projects/achd-data/'

################################## LOAD DATA ############################################

#import ACHD data
achd <- readRDS(file = paste(pt_data, 'output/rpah_analysis_dataset.rds', sep=""))

#import DX coding
dx_codes <- read.csv(file = here("ACHD_Dashboard", "data", "ACHD_EPCC_coding.csv"), fileEncoding="UTF-8-BOM") %>% 
            filter(!(Variable == 'adm_AbsentPA' | Variable == 'PA' | Variable == "achd_id")) %>%
            select(!c(ACHD_Database_name, check., Variable))

#import Census data
sa2.TB <- readRDS(file = here("ACHD_Dashboard", "data", "sa2_demographics.rds")) %>%
                            mutate(IRSD = ordered(IRSD, c(1,2,3,4,5,6,7,8,9,10)),
                                   IRSAD = ordered(IRSAD, c(1,2,3,4,5,6,7,8,9,10)))

#import htt data
htt.nsw <- readRDS(file = here("ACHD_Dashboard", "data", "htt_nsw.rds")) # Driving times data
htt.details <- readRDS(file = here("ACHD_Dashboard", "data", "htt_details.rds")) # Hospital Metadata

#current achd clinics
achd_ids <- c(152, 408, 646, 683, 737, 979)
#placeholer for adding new clinics
new_achd_ids <- achd_ids

#Import area polygons
sa2.polys <- readOGR(here("ACHD_Dashboard", "data", "ASGS", "sa2", "sa2_polys.shp"))

############################# HEADER CONTENT #######################################

header <- dashboardHeader(title = "Clinic Planning Tool")


############################# SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Welcome Page
        menuItem("Welcome", tabName = "welcome", icon = icon("info")),
        # Snapshot tab
        menuItem("Snapshot of ACHD", tabName = "snapshot", icon = icon("file-medical")),
        # Driving tab
        menuItem("Clinic Planning", tabName = "driving", icon = icon("map-marker-alt")),
        
        # Report Download Button
        h4("Download Report"),
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
        ############################# Welcome Tab ################################
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
                      HTML(
                      "<p>
                      <b>Please Click the “Load Patient Data” in the top right before beginning
                      <br>
                      <br>You can navigate between the pages on the sidebar on the right. The main tool can be found on the tab 
                      named “Clinic Planning”.
                      <br>
                      <br>The tab “Snapshot of ACHD” provides an overview of the patient level data. 
                      <br>
                      <br>You can filter the patient level data using the Global Filters in the sidebar.</b>
                      <br>
                      <br>This tool has been developed to enable evidence generation for the planning of Adult Congenital Heart Disease 
                      (ACHD) Clinics in New South Wales (NSW). ACHD has become a priority population due to its growing population, 
                      complex care needs and the requirement ‘whole-of-life’ follow up. A national action plan for Congenital Heart 
                      Disease has been developed by the federal government to address these issues and the ACHD Service at Royal 
                      Prince Alfred Hospital (RPAH) has set up a rural outreach program to improve follow up rates and clinic 
                      engagement for ACHD patients in NSW.
                      <br>
                      <br>The “Clinic Planning” page is designed to aid the clinic planning effort of the ACHD service in NSW by 
                      providing an interactive environment where clinicians can explore how the characteristics of ACHD population 
                      in NSW. It enables the visualisation of patient locations, area-level demographic and disease characteristics, 
                      information about the current and new ACHD clinic locations and driving times to clinics. Using the clinic 
                      selector, potential locations for new ACHD clinics can be added to the map to determine their impact.
                      </p>"
                      )
                    
                  ),
                  box(title = "Data Sources",
                      HTML(
                      "
                      <ol>
                      <li><i>Adult Congenital Heart Disease database:</i> Contains records from ACHD patients with an encounter at 
                      RPAH.</li>
                      <br>
                      <li><i>Congenital Heart Disease Diagnostic Coding Reference Table:</i> Developed by the Congenital Heart 
                      Alliance of Australia and New Zealand, this coding reference table was used to standardise diagnosis codes 
                      to the European Paediatric Congenital Cardiology – Short List coding convention.</li>
                      <br>
                      <li><i>Australian Bureau of Statistics - Australian Geographical Data Standard (ASGS):</i> Geographical 
                      boundaries that contain census data, providing area level demographic information (ABS, 2018).</li>
                      <br>
                      <li><i>Australia Bureau of Statistics – Census 2016:</i> Area level census data has been downloaded from the 
                      Table Builder tool to include key demographic information.</li>
                      <br>
                      <li><i>Travel Times to Hospitals in Australia:</i> A dataset that determines the travel time by car between ABS
                      census areas and hospitals in Australia (Barbieri & Jorm, 2019).</li>
                      </ol>
                      "
                      )
                  ),
                ),
                fluidRow(
                  box(title = "Key Definitions",
                      HTML(
                      "
                      <ol>
                      <li><i>Patient Locations:</i> Patient locations for will be defined by addresses taken from the RPAH.</li>
                      <br>
                      <li><i>Statistical Area 2 (SA2):</i> This is a geographic boundary defined by the ASGS, representing a community that 
                      interacts together socially and economically, with an average population of 10,000 people.</li>
                      <br>
                      <li><i>Disease severity:</i> The Bethesda classification has been used in the tool, where ACHD diagnosis are grouped 
                      into ‘mild’, ‘moderate’ and ‘severe’ as described by Warnes et al. (2008).</li>
                      <br>
                      <li><i>Accessibility to ACHD clinics:</i> Driving distance to the nearest ACHD clinic for each SA2 area will be 
                      calculated using the Travel Times to Hospitals in Australia dataset.</li>
                      <br>
                      <li><i>Loss to Follow up:</i> A patient has been determined as lost to follow up if they have not attended an RPAH 
                      associated clinic in 3 or more years.</li>
                      <br>
                      <li><i>Level of Disadvantage:</i> this is the “Index of Relative Socio-economic Disadvantage” defined by the ABS, a 
                      score of 1 is more disadvantages and 10 is least disadvantaged.</li>
                      <br>
                      <li><i>Remoteness:</i> This is the “Accessibility and Remoteness Index of Australia” defined by the ABS, It contains 5 
                      levels, Major Cities, Inner Regional, Outer Region, Remote and Very Remote. This has been summarised into three 
                      levels for this tool, Cities, Regional and Remote.</li>
                      <br>
                      <li><i>Global Filters:</i> These options will filter the patient level dataset from the ACHD database, affecting all 
                      the area level information as well.</li>
                      </ol>
                      "
                      )
                  ),
                  box(title = "References",
                      HTML(
                        "
                        <ol>
                        <li>ABS. (2018). Australian Statistical Geography Standard (ASGS). Retrieved from 
                        https://www.abs.gov.au/websitedbs/D3310114.nsf/home/<br>Australian+Statistical+Geography+Standard+(ASGS)</li>
                        <br>
                        <li>Barbieri, S., & Jorm, L. (2019). Travel times to hospitals in Australia. Scientific Data, 6(1), 248. 
                        doi:10.1038/s41597-019-0266-4</li>
                        <br>
                        <li>Warnes, C. A., Williams, R. G., Bashore, T. M., Child, J. S., Connolly, H. M., Dearani, J. A., . . . Webb, 
                        G. D. (2008). ACC/AHA 2008 Guidelines for the Management of Adults With Congenital Heart Disease; A Report 
                        of the American College of Cardiology/American Heart Association Task Force on Practice Guidelines (Writing 
                        Committee to Develop Guidelines on the Management of Adults With Congenital Heart Disease): Developed in 
                        Collaboration With the American Society of Echocardiography, Heart Rhythm Society, International Society for 
                        Adult Congenital Heart Disease, Society for Cardiovascular Angiography and Interventions, and Society of 
                        Thoracic Surgeons. J Am Coll Cardiol, 118(23), e714-e833. 
                        doi:10.1161/circulationaha.108.190690</li>
                        "
                      )
                    
                    
                  )
                )
        ),
        
        ############################# Snapshot Tab ################################
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
                         uiOutput("ltf_box")
                ),
                fluidRow(
                  box(uiOutput("sex_box"), width = 4),
                  box(uiOutput("ndx_box"), width = 4),
                  box(uiOutput("nclinic_box"), width = 4)
                ),
                fluidRow(
                  uiOutput("dx_box")
                  
                )
        ),
        
        ############################# Driving Tab ################################
        tabItem(tabName = "driving",
                # Instructions for using driving tab
                box(title = "Instructions", collapsible = T, width = 12,
                    HTML(
                    "
                    <p>
                    <b>Map Customisation</b><br>
                    The first option ‘Filter by Region’ allows you to specifically choose geographical areas based on larger region, 
                    by default all regions are selected. Deselecting highly populated areas (‘Greater Sydney’ and ‘ACT’) can help to 
                    explore less populated areas (‘Rest of NSW’) in more details. There are two overlay options that can be places on the map. The first ‘Driving Time to Nearest Clinic’ will 
                    display a choropleth of the driving time from each area to the nearest ACHD Clinics. The second, ‘Total ACHD 
                    Population’ will display a choropleth of the number of ACHD patients in each area.<br>
                    <br>
                    <b>Clicking the ‘Update’ button will load the app and add any updates or changes that you have selected.</b><br>
                    <br>
                    <b>New Clinic Selection</b><br>
                    This section will allow you to choose hospitals for new clinic locations. First select the desire Primary 
                    Health Network Area, this will allow you to choose the Local Hospital Network area which will in turn allow 
                    you to select a hospital from that area. Click “Add Clinic” to add the new hospital to the list and “Reset 
                    Clinics” to remove all the selected hospitals. Once you have chosen all the desired hospital locations, click 
                    ‘update’ in the Map Customisation box to add the clinics to the map.<br>
                    <br>
                    <b>Clinics Tables</b><br>
                    There are two tabs in this section, “Current Clinics” provides information about the current ACHD Clinics and 
                    “New Clinics” provide information about the newly selected clinics.<br>
                    <br>
                    <b>The Map and Area Information</b><br>
                    Hovering over area on the map will provide more information about that area and clicking the area will display 
                    more details to the left of the map. Once the area information is displayed you will be provided with an option 
                    to add that area summary to the downloadable report. Click the ‘Add area summary to report’ button to add this 
                    area information to the report. This can be done sequentially to add multiple area summaries to the report.<br>
                    </p>
                    "
                    )
                    ),
                
                
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
                             width = 8, height = 580),
                         column(width = 4,
                             fluidRow(uiOutput('area.output'), 
                                      width = 12,  
                                      height = 650),
                         )
                ),
                
                fluidRow(uiOutput('area.dx.box') # Diagnoses Present in selected area
                )
        )
        
    )
)


############################# UI OUTPUT #######################################
# Define UI for application
ui <- dashboardPage(header, sidebar, body)

############################# SERVER #######################################

# Define server logic
server <- function(input, output) {
    
    # Function to search for input names
    getInputs <- function(pattern){
        reactives <- names(reactiveValuesToList(input))
        reactives[grep(pattern,reactives)]
    }
    
############################# GLOBAL REACTIVE FUNCTIONS #################################
    
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
            select(dx_codes$EPCC_Code) %>%
            mutate_all(as.character) %>%
            mutate_all(as.numeric) %>%
            summarise_all(sum, na.rm = TRUE) %>%
            t() %>% as.data.frame() %>% 
            rename("dx_count" = V1) %>% 
            mutate(dx_label = dx_codes$Adjust_name[match(row.names(.), dx_codes$EPCC_Code)])
        
        dx.df
    })

    # prepare area level data
    drive.area.achd <- reactive({
      
    # Diagnosis severity counts in each area
    beth.drive <- achd.filtered() %>%
        group_by(sa2) %>%
        dplyr::summarise(ACHD_count = n(), # Number of ACHD patients in each area
                         beth_1 = sum(bethesda_code == 1), # Number with simple CHD in each area
                         beth_2 = sum(bethesda_code == 2), # Number with moderate CHD in each area
                         beth_3 = sum(bethesda_code == 3), # Number with complex CHD in each area
                         beth_4 = sum(bethesda_code == 4), # Number with unknown complexity in each area
                         ltf_3 = sum(ltf_3), # Number of patients not seen for 3 years in each area
                         ltf_4 = sum(ltf_4), # Number of patients not seen for 4 years in each area
                         ltf_5 = sum(ltf_5)) # Number of patients not seen for 5 years in each area
    
    # Diagnoses present in each area 
    dx.drive <- achd.filtered() %>%
        mutate_at(as.character(dx_codes[['EPCC_Code']]), as.character) %>% 
        mutate_at(as.character(dx_codes[['EPCC_Code']]), as.numeric) %>%
        group_by(sa2) %>% 
        dplyr::summarise_at(as.character(dx_codes[['EPCC_Code']]), sum, na.rm = TRUE)
    
    # Join above to table builer data
    area.drive <- left_join(sa2.TB, beth.drive, by = c('SA2_NAME' = 'sa2')) %>% 
        left_join(dx.drive, by = c('SA2_NAME' = 'sa2')) %>%
        mutate_at(as.character(dx_codes[['EPCC_Code']]), function(x) replace(x, is.na(x), 0)) %>%
        mutate_at(c('ACHD_count', 'beth_1', 'beth_2', 'beth_3', 'beth_4',
                    'ltf_3', 'ltf_4', 'ltf_5'), function(x) replace(x, is.na(x), 0))
    
    area.drive
    })
    
############################# SIDEBAR #################################
    
    #-------------------- UI elements for Global Fitlers--------------#
    # Filter by Disease Severity
    output$out.bethesda <- renderUI ({
        checkboxGroupInput("sb.bethesda", "Disease severity",
                           choices = c('Simple' = 1,
                                       'Moderate' = 2,
                                       'Complex' = 3,
                                       'Unknown' = 4),
                           selected = c(1, 2, 3))
    })
    
    # Filter by Sex
    output$out.sex <- renderUI ({
        checkboxGroupInput("sb.sex", "Sex",
                           choices = c('Male' = 1,
                                       'Female' = 2,
                                       'Neither' = 4),
                           selected = c(1, 2, 4))
    })
    
    # Filter by Mortality
    output$out.mortality <- renderUI ({
        checkboxGroupInput("sb.mortality", "Deceased patients",
                           choices = c('Alive' = 0,
                                       'Deceased' = 1),
                           selected = c(0))
    })
    
    # Filter by Age
    output$out.age <- renderUI ({
        sliderInput('sb.age', 'Age',
                    min = 18,
                    max = 110,
                    value = c(18, 110),
                    round = TRUE)
    })
    
    # Filter by Time Period
    output$out.dates <- renderUI ({
        dateRangeInput("sb.dates", "Select a time period:",
                       start = "2000-01-01", end = "2020-12-31",
                       min = "2000-01-01", max = "2020-12-31",
                       format = "dd/mm/yyyy")
    })
    
    # Filter by Time since last visit   
    output$out.last.clinic <- renderUI ({ 
        sliderInput('sb.last.clinic', 'Time since last clinic visit',
                    min = 0,
                    max = 21,
                    value = c(0,21),
                    round = TRUE)
    })
        
    #-------------------- Resetting the Global Filters--------------#
    # When the rest button is clicks turn the following to default
    observeEvent(input$sb.reset, {
        # Disease Severity
        output$out.bethesda <- renderUI ({
            checkboxGroupInput("sb.bethesda", "Disease severity",
                               choices = c('Simple' = 1,
                                           'Moderate' = 2,
                                           'Complex' = 3,
                                           'Unknown' = 4),
                               selected = c(1, 2, 3))
        })
        # Sex
        output$out.sex <- renderUI ({
            checkboxGroupInput("sb.sex", "Sex",
                               choices = c('Male' = 1,
                                           'Female' = 2,
                                           'Neither' = 4),
                               selected = c(1, 2, 4))
        })
        # Mortality
        output$out.mortality <- renderUI ({
            checkboxGroupInput("sb.mortality", "Deceased patients",
                               choices = c('Alive' = 0,
                                           'Deceased' = 1),
                               selected = c(0))
        })
        # Age
        output$out.age <- renderUI ({
            sliderInput('sb.age', 'Age',
                        min = 18,
                        max = 110,
                        value = c(18, 110),
                        round = TRUE)
        })
        # Time Period
        output$out.dates <- renderUI ({
            dateRangeInput("sb.dates", "Select a time period:",
                           start = "2000-01-01", end = "2020-12-31",
                           min = "2000-01-01", max = "2020-12-31",
                           format = "dd/mm/yyyy")
        })
        # Time Since Last Visit
        output$out.last.clinic<- renderUI ({ 
            sliderInput('sb.last.clinic', 'Time since last clinic visit',
                        min = 0,
                        max = 21,
                        value = c(0,21),
                        round = TRUE)
        })
    })
    
############################# SNAPSHOTS TAB #################################
    
    ############################# Value Boxes #################################
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
    
    
    ############################# Plotting ########################
    # Age histogram
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
                     output$age_box <- renderUI({
                       box(plotOutput("age_plot",
                                      height = 200))
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
    
    # Time since last clinic plot
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
                     output$ltf_box <- renderUI({
                       box(plotOutput("ltf_plot",
                                      height = 200))
                     })
                   })
    output$ltf_plot <- renderPlot({
      ggplot(achd.filtered(), aes(x=as.numeric(gap_2000, "years"))) +
        geom_histogram(fill = "light grey", color = "black") +
        labs(y = "Number of Patients", 
             x = "Time since last clinic visit (yrs)") +
        theme(axis.title.x=element_blank()) +
        theme_minimal()
    })
    
    
    # Sex bar plot
    observeEvent(c(input$sb.update,
                   input$load.data), ignoreInit = T, {
                     output$sex_box <- renderUI({
                       plotOutput("sex_plot",
                                  height = 200)
                       
                     })
                   })
    output$sex_plot <- renderPlot ({
      achd.filtered() %>% filter(!is.na(sex)) %>%
        ggplot(aes(x=sex)) +
        geom_bar(fill = "light grey", color = "black") +
        scale_x_discrete(name = "Sex",
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
                       plotOutput("ndx_plot",
                                  height = 200)
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
                       plotOutput("nclinic_plot",
                                  height = 200)
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
                       div(style = 'overflow-y:scroll;height:500px;',
                           box(plotOutput("dx_plot", height = 1500), 
                               width = 12, height = 1500)
                       )
                     })
                   })
    output$dx_plot <- renderPlot({
      dx.count() %>% filter(dx_count > 0) %>% 
        ggplot(aes(x = reorder(dx_label, dx_count), y=dx_count)) + 
        geom_bar(stat="identity", fill = "light grey", color = "black", size = 0.25) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Frequecy of each diagnosis", 
             y = "count",
             x = "") +
        coord_flip() +
        theme_minimal()
    })

############################# DRIVING TAB #################################
 
    ######################### Reactive Values for Driving Map #############
    drive.values <- reactiveValues()
    #For creating and editing the clinics table
    drive.values$add_clinic_table <- data.frame(Hospital = as.character(), 
                                                Patients = as.character(),
                                                ltf = as.character(),
                                                stringsAsFactors = FALSE)
    #For tracking the ACHD clinic IDs selected
    drive.values$new_achd_ids <- c(646, 755, 152, 683, 737, 979)
    #For adding new areas for report summary
    drive.values$area.report.list <- list()
    #For the area level data
    observe({ drive.values$area_data <- drive.area.achd() })
    #To store map clicks
    drive.values$map.clicks <- c()
    
    ######################### Building the Current Clinics Table ####################
    
    observeEvent(
      
      c(input$sb.update,
        input$load.data), ignoreInit = T, # Triggers on either load data, or update filters
      
      {
    
    # Create driving data table for the current clinics
    current_hospitals <- htt.nsw %>%
                          select(SA2_5DIGIT, as.character(achd_ids)) %>% # Select the relevant hospitals, and the area they are in
                          mutate_at(as.character(achd_ids), as.duration) %>% # Convert driving time data to duration type
                          left_join(drive.area.achd(), by ='SA2_5DIGIT') %>% # Join by sa2 area, adding area level data
                          select(as.character(achd_ids), SA2_NAME, SA2_5DIGIT, ACHD_count, 
                                 ltf_3, beth_1, beth_2, beth_3) # Select Relvant columns
    
    # Make a table summarise the current clinics
    drive.values$current_clinic_table <- data.frame(
                      # Hospital Names
                      Hospital = htt.details %>% filter(Hospital_ID %in% achd_ids) %>% pull(Hospital.name),
                      # Numer of Patients within 1 hour drive
                      Patients = sapply(achd_ids, function(x) current_hospitals %>% 
                                                              filter(as.numeric(!!as.symbol(x), "hours") <= 1) %>%
                                                              summarise(hr_drive = sum(ACHD_count)) %>%
                                                              pull(hr_drive) %>% as.integer()),
                      # Number of Patients Lost to Follow up within 1 hour drive
                      ltf = sapply(achd_ids, function(x) current_hospitals %>% 
                                                         filter(as.numeric(!!as.symbol(x), "hours") <= 1) %>%
                                                         summarise(hr_drive = sum(ltf_3)) %>%
                                                         pull(hr_drive) %>% as.integer()),
                      stringsAsFactors = FALSE)
    
    # Output the table
    output$current.clinic.output <- renderUI({
      output$current.clinics.table <- renderTable(drive.values$current_clinic_table)
      tableOutput("current.clinics.table")
    })
    
      })
    
    ######################### When 'Add Clinic' Button is clicked ####################
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
      
      # Get driving information for selected hospital
      selected_hospital <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(hospital_id)) %>%
        mutate(hospital = as.duration(.[[as.character(hospital_id)]])) %>%
        left_join(drive.values$area_data, by ='SA2_5DIGIT') %>%
        select(hospital, SA2_NAME, SA2_5DIGIT, ACHD_count, ltf_3, beth_1, beth_2, beth_3)
      
      # New row to add to clinic table
      new.row <- isolate(data.frame(
                 # Hospital Name
                 Hospital = input$hospital,
                 # Hospital ID
                 ID = hospital_id,
                 # Number of Patients withing 1 hour Drive
                 Patients = selected_hospital %>% filter(as.numeric(hospital, "hours") <= 1) %>%
                                                  summarise(hr_drive = sum(ACHD_count)) %>%
                                                  pull(hr_drive) %>% as.integer(), 
                 # Number of Patients Lost to Follow up within 1 hour drive
                 ltf = selected_hospital %>% filter(as.numeric(hospital, "hours") <= 1) %>%
                                             summarise(hr_ltf = sum(ltf_3)) %>%
                                             pull(hr_ltf) %>% as.integer(),
                 stringsAsFactors = FALSE))
      
      # Add the new row to the clinic table, if it wasnt already selected
      if ( !(input$hospital %in% isolate(drive.values$add_clinic_table$Hospital)) )
      { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row)) }
      
      # Display the new clinics table
      output$new.clinics.output <- renderUI({
        output$new.clinics.table <- renderTable(drive.values$add_clinic_table %>%
                                                    select(-ID)) # Do not need to display the Hospital ID number
        tableOutput("new.clinics.table")
      })
      
      
    })
    
    ######################### When 'Reset Clinics' Button is clicked ####################
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
    
    ######################### Value Boxes ###############################################
    # Value boxes with the key data points
    observeEvent(input$drive.update, {
    # Total Number of Patients
    output$pt.count.drive <- renderValueBox({
        valueBox(tags$p(achd.filtered() %>% nrow(),
                        style = "font-size: 75%;"),
                 'selected patients', 
                 width = 3, color = 'light-blue')
    })
    # Breakdown is disease severity
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
    # Total Number of Patients Lost to Follow up
    output$ltf.count.drive <- renderValueBox({
        valueBox(tags$p(sum(achd.filtered()$ltf_3),
                        style = "font-size: 75%;"), 
                 "Patients Lost to Follow up", 
                 color = 'light-blue')
    })
    # Total Number of Patients within 1 hour drive
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
    
    
    ######################### UI for selecting clinics ###################################
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
    
    ######################### Hospital Location Markers ###################################
    
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
    
    ######################### Reactive Functions for Mapping ###############################
    
    # sa2 polys for driving map
    drive.polys <- eventReactive(input$drive.update, {
        #add area data to polygons
        drive.poly <- merge(sa2.polys, drive.values$area_data, by.x = 'NAME16', by.y = 'SA2_NAME')
        
        # Filter by the GCC areas selected
        polys.filtered <- subset(drive.poly, drive.poly$GCC_NAME16 %in% input$drive.gcc)
        polys.filtered
    })
    
    # get coords function to set map view
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
    
    # Set bins for driving overlay
    bins.drive <- eventReactive(input$drive.update, {
        bins <- seq(0, 
                    plyr::round_any(max(as.numeric(drive.polys()$shortest_time, 'hours'), na.rm = TRUE), 10, ceiling), 
                    plyr::round_any(max(as.numeric(drive.polys()$shortest_time, 'hours'), na.rm = TRUE), 10, ceiling)/10)
        bins
    })
    
    # Set colour palette for driving overlay
    pal.drive <- eventReactive(input$drive.update, {
        pal <- colorNumeric("YlOrRd", domain = as.numeric(drive.polys()$shortest_time, 'hours'))
        pal
    })
    
    #Set bins for locations overlay
    bins.loc2 <- reactive({
        bins <- seq(0, 
                    plyr::round_any(max(drive.polys()$ACHD_count), 10, ceiling), 
                    plyr::round_any(max(drive.polys()$ACHD_count), 10, ceiling)/10)
        bins
    })
    
    #Set colour palette for locations overlay
    pal.loc2 <- reactive({
        pal <- colorNumeric("YlOrRd", domain = drive.polys()$ACHD_count)
        pal
    })
    
    # Set labels area mouse over
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
    
    ######################### Base Map ###############################
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
    
    ######################### Observers for Map editting ###############
    
    # Adding polygons
    observeEvent(input$drive.update, {
        
        if (input$select.overlay == 'drive.overlay') {
        #Driving Overlay polygons
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
            # Locations Overlay Polygons
            leafletProxy("drive.map") %>% 
                # Clear old polygons
                clearShapes %>%
                # Add new polygons
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
            # Legend for Driving Map Overlay
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
            #Legend for Locations map overlay
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
      
        # If new clinics have been selected add new markers
        if ( length(drive.values$add_clinic_table$Hospital) != 0) {
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
          # If no new clinics have been selected, clear all the new clinic markers
        } else {
            leafletProxy("drive.map") %>%
                # Clear old markers 
                clearGroup(group = "new_markers")
        }
        
    })
    
    ######################### Diaplay Information of Area Click ###############
    observeEvent( input$drive.map_shape_click, { 
      #---------------DATA SETUP-----------------#
      # the area click event
      event <- input$drive.map_shape_click
      # Add the click to the reactive list
      if ( !(event$id %in% drive.values$map.clicks) )
      { drive.values$map.clicks <- c(drive.values$map.clicks, event$id) }
      
      print(drive.values$map.clicks)
      # select the correct area
      drive.values$event_area <- drive.values$area_data %>% filter(SA2_5DIGIT %in% drive.values$map.clicks)
      
      #---------------AREA SUMMARY-----------#
      output$area.output <- renderUI({
        div(style = 'overflow-y:scroll;height:580px;',
        box(h2("Area Focus"),
            div(style="display:inline-block",actionButton('area.button', 'Add to report')),
            div(style="display:inline-block",actionButton("area.reset", "Reset")),
            HTML(paste(
            "
            <br><br>
            <b>Selected Areas:</b> ", paste(drive.values$event_area$SA2_NAME, collapse = ', '), "<br>
            <br>
            <b>Number of ACHD Patients:</b> ", sum(drive.values$event_area$ACHD_count),"<br>
            <br>
            <b>Number with:</b> <br>
            <ul>
              <li>Simple CHD: ",sum(drive.values$event_area$beth_1),"</li>
              <li>Moderate CHD: ",sum(drive.values$event_area$beth_2),"</li> 
              <li>Complex CHD: ",sum(drive.values$event_area$beth_3),"</li>
            </ul>
            <b>Number of Patients lost to follow up:</b><br> 
            <ul>
              <li>",sum(drive.values$event_area$ltf_3),"</li>
            </ul>
            <b>Index of Relative Socio-economic Disadvantage</b><br>
              (1 = most disadvantaged, 10 = least disadvantaged): <br>
              <ul>",
              paste("<li>", drive.values$event_area$SA2_NAME, ": ", 
                    drive.values$event_area$IRSD, "</li>", collapse = ""),
            "</ul>
            <b>Aboriginal and Torres Strait Islander Population</b><br>
            <ul>
                <li>Total: ",sum(drive.values$event_area$atsi),"</li>
                <li>Percent: ",percent(sum(drive.values$event_area$atsi) / 
                                       sum(drive.values$event_area$total_pop)),"</li>
            </ul>
            <b>Accessibility and Remoteness Index of Australia</b><br>
            (percentage of population living in):<br>  
            <ul>
                <li>Major Cities: ",percent(sum(drive.values$event_area$major_cities) / 
                                    length(drive.values$event_area$SA2_NAME)),"</li>
                <li>Inner Regional:",percent(sum(drive.values$event_area$inner_regional) / 
                                     length(drive.values$event_area$SA2_NAME)),"</li>
                <li>Outer Regional:",percent(sum(drive.values$event_area$outer_regional) / 
                                     length(drive.values$event_area$SA2_NAME)),"</li> 
                <li>Remote:",percent(sum(drive.values$event_area$remote) / 
                             length(drive.values$event_area$SA2_NAME)),"</li>
                <li>Very Remote:",percent(sum(drive.values$event_area$very_remote) / 
                                  length(drive.values$event_area$SA2_NAME)),"</li>
            </ul>
            <b>Driving time to nearest clinic:</b><br> 
            <ul>",
                 paste("<li>",drive.values$event_area$SA2_NAME,":</li>  
                 <ul>
                    <li>Current Locations: ", 
                    round(as.numeric(drive.area.achd()$shortest_time[drive.area.achd()$SA2_5DIGIT %in% drive.values$map.clicks], 'hours'), 
                    digits = 2)," hrs</li>
                    <li>New Locations: ", 
                    round(as.numeric(drive.values$event_area$shortest_time, 'hours'), digits = 2)," hrs</li>
                 </ul>", collapse = ""),   
            "</ul>
            ")
            ),
            width = 12
        )
        )
      })
      
      #---------------AREA DIAGNOSES-----------#
      # Box to display area diagnoses
      output$area.dx.box <- renderUI({
        box(title = "CHD Diagnoses in Selected Areas",
            plotOutput("dx_area_plot"),
            width = 12, height = 580
        )
      })
      
      # Plot of Diagnoses present in area
      output$dx_area_plot <- renderPlot({
        dx.area.data <- drive.values$event_area %>% select(dx_codes$EPCC_Code) %>% 
                                  mutate_all(as.character) %>%
                                  mutate_all(as.numeric) %>%
                                  summarise_all(sum, na.rm = TRUE) %>%
                                  t() %>% as.data.frame() %>%
                                  rename("dx_count" = V1) %>% 
                                  mutate(dx_label = dx_codes$Adjust_name[match(row.names(.), dx_codes$EPCC_Code)]) %>%
                                  filter(dx_count > 0)
        
        ggplot(data = dx.area.data, aes(x = reorder(dx_label, dx_count), y=dx_count)) + 
          geom_bar(stat="identity") +
          scale_y_continuous(breaks = seq(0, max(dx.area.data$dx_count), 1)) +
          theme(axis.text.x = element_text(angle = 90)) +
          labs(title = "Frequecy of each diagnosis", 
               y = "count",
               x = "") +
          coord_flip()
      })
      
    })
    
    #---------------ADD AREA SUMMARY TO REPORT-----------#
    # Adding the area summary to the report (Really we are just adding the vector or area ids to a list)
    observeEvent(input$area.button, {
      # Check is vector is already in list before adding
      if ( !(Position(function(x) 
        identical(x, drive.values$map.clicks), 
        drive.values$area.report.list, nomatch = 0) > 0) )
        # Add to list if not already there  
      { drive.values$area.report.list[[length(drive.values$area.report.list) + 1]] <- list(drive.values$map.clicks) }
    })
    
    #---------------RESET AREA SELECTION-----------#
    observeEvent(input$area.reset, {
      #clear area report
      output$area.output <- renderUI(div(style = 'overflow-y:scroll;height:580px;',
                                         box(title = "Select New Areas",
                                             width = 12)))
      # Clear the area dx plot
      output$area.dx.box <- renderUI({
        box(title = "Select New Areas")
      })
      
      #reset the map clicks
      drive.values$map.clicks <- c()
      
    })
    
    
############################# DOWNLOAD REPORT #################################
    output$report.dl <- downloadHandler(
      # Report Filename
      filename = paste(Sys.Date(), "ACHD_Report.html", sep = "_"),
      
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReport <- file.path(tempdir(), "Report.Rmd")
        tempAreaReport <- file.path(tempdir(), "Area_Report.Rmd")
        tempClinicReport <- file.path(tempdir(), "New_Clinic_Report.Rmd")
        
        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
        file.copy("Area_Report.Rmd", tempAreaReport, overwrite = TRUE)
        file.copy("New_Clinic_Report.Rmd", tempClinicReport, overwrite = TRUE)
        
        rmarkdown::render(tempReport, output_file = file)
        
      }
      
    )
    
}

############################# RUN APP #################################

# Run the application 
shinyApp(ui = ui, server = server)
