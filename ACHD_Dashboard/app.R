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
library(shinyWidgets)
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


################################## LOAD DATA ############################################
#import Census data
sa2.TB <- readRDS(file = here("ACHD_Dashboard", "data", "sa2_demographics.rds")) %>%
                            mutate(IRSD = ordered(IRSD, c(1,2,3,4,5,6,7,8,9,10)),
                                   IRSAD = ordered(IRSAD, c(1,2,3,4,5,6,7,8,9,10)))

#import htt data
htt.nsw <- readRDS(file = here("ACHD_Dashboard", "data", "htt_nsw.rds")) # Driving times data
htt.details <- readRDS(file = here("ACHD_Dashboard", "data", "htt_details.rds")) # Hospital Metadata

#Import area polygons
sa2.polys <- readOGR(here("ACHD_Dashboard", "data", "ASGS", "sa2", "sa2_polys.shp"))

############################# HEADER CONTENT #######################################

header <- dashboardHeader(title = "Clinic Planning Tool")


############################# SIDEBAR CONTENT #######################################

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Welcome Page
        menuItem("Welcome", tabName = "welcome", icon = icon("info")),
        # Driving tab
        menuItem("Clinic Planning", tabName = "driving", icon = icon("map-marker-alt")),
        
        # Report Download Button
        h4("Download Report"),
        downloadButton("report.dl", "Download", icon=icon("download")),
        tags$head(tags$style(".dl_butt{color:blue;}"))
    )
)


############################# BODY CONTENT #######################################
body <- dashboardBody(
    tabItems(
        ############################# Welcome Tab ################################
        tabItem(tabName = "welcome",
                fluidRow(
                  
                  # Title bar
                  valueBox("Clinic Planning in Australia",
                           "Combining geographic information systems, census data and driving times to hospitals to determine
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
                      <li><i>Statistical Area 2 (SA2):</i> This is a geographic boundary defined by the ASGS, representing a community that 
                      interacts together socially and economically, with an average population of 10,000 people.</li>
                      <li><i>Level of Disadvantage:</i> this is the “Index of Relative Socio-economic Disadvantage” defined by the ABS, a 
                      score of 1 is more disadvantages and 10 is least disadvantaged.</li>
                      <br>
                      <li><i>Remoteness:</i> This is the “Accessibility and Remoteness Index of Australia” defined by the ABS, It contains 5 
                      levels, Major Cities, Inner Regional, Outer Region, Remote and Very Remote. This has been summarised into three 
                      levels for this tool, Cities, Regional and Remote.</li>
                    
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
                        "
                      )
                    
                    
                  )
                )
        ),
        
        
        ############################# Driving Tab ################################
        tabItem(tabName = "driving",
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
                      uiOutput('overlay.selector'),
                      actionButton("drive.update", "Update"),
                      width = 4, height = 340
                  ),
                  
                  # Selecting New Clinics
                  tabBox(title = NULL,
                         tabPanel("Select Hospitals by location",
                                  uiOutput('phn.selector'), # Primary Health Network
                                  uiOutput('lhn.selector'), # Local Health Network
                                  uiOutput('hospital.selector'), # Hospital
                                  # Add hospital to new clinics list
                                  div(style="display:inline-block",uiOutput('clinic.button.add')),
                                  # Reset Clinics
                                  div(style="display:inline-block",uiOutput('clinic.button.reset')),
                         ),
                         tabPanel("Filter Hospitals by Type",
                                  pickerInput(
                                    inputId = 'description.filter',
                                    label = 'Filter Hospitals by Description',
                                    choices = unique(htt.details$Description),
                                    options = list(
                                      `actions-box` = TRUE,
                                      `live-search` = TRUE,
                                      `selected-text-format` = "count > 3"), 
                                    multiple = TRUE
                                  ),
                                  pickerInput(
                                    inputId = 'sector.filter',
                                    label = 'Filter Hospitals by Sector',
                                    choices = unique(htt.details$Sector),
                                    options = list(
                                      `actions-box` = TRUE), 
                                    multiple = TRUE
                                  ),
                                  pickerInput(
                                    inputId = 'beds.filter',
                                    label = 'Filter Hospitals by Number of Beds',
                                    choices = unique(htt.details$Beds),
                                    options = list(
                                      `actions-box` = TRUE,
                                      `selected-text-format` = "count > 3"), 
                                    multiple = TRUE
                                  ),
                                  # Add hospital to new clinics list
                                  div(style="display:inline-block",actionButton("filter.add", "Add Clinics")),
                                  # Reset Clinics
                                  div(style="display:inline-block",actionButton("filter.reset", "Reset Clinics")), # Reset list
                         ),
                         width = 4, height = 340
                  ),
                  
                  # Information about Hospital Coverage
                  tabBox(title = NULL,
                         id = "clinic.tabs", 
                         tabPanel("Current Clinics", uiOutput("current.clinic.output")), # Current hospitals
                         tabPanel("New Clinics", uiOutput("new.clinics.output")), # Newly Selected Hospitals
                         width = 4, height = 340),
                ),
                
                # Map and Area level value boxes
                fluidRow(box(leafletOutput('drive.map', height = 550),
                             width = 8, height = 580),
                         column(width = 4,
                             fluidRow(uiOutput('area.output'), 
                                      width = 12,  
                                      height = 650),
                         )
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

############################# DRIVING TAB #################################
 
    ######################### Reactive Values for Driving Map #############
    drive.values <- reactiveValues()
    # For creating and editing the clinics table
    drive.values$add_clinic_table <- data.frame(Hospital = as.character(),
                                                ID = as.character(),
                                                stringsAsFactors = FALSE)
    # For tracking the current clinic IDs
    drive.values$current_clinic_ids <- c()
    # For tracking the new clinic IDs selected
    drive.values$new_clinic_ids <- c()
    # For adding new areas for report summary
    drive.values$area.report.list <- list()
    # For the area level data
    observe({ drive.values$area_data <- sa2.TB })
    # To store map clicks
    drive.values$map.clicks <- c()
    
    ######################### When 'Add Clinic' Button is clicked in the location selector ####################
    observeEvent(input$location.add, {
        
      # The hospital ID that was selected
      hospital_id <- htt.details$Hospital_ID[htt.details$Hospital.name == input$hospital]
      
      # Add the new hospital id to the clinics id list
      if ( !(hospital_id %in% drive.values$new_clinic_ids) ) 
      { drive.values$new_clinic_ids <- append(drive.values$new_clinic_ids, hospital_id) }
      
      print(drive.values$new_clinic_ids)
      
      # Use the new clinics id list to calculate the driving time to the nearest hospital
      shortest_time_new <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(drive.values$new_clinic_ids)) %>%
        mutate(shortest_time = pmap_dbl(
          .l = select(., -SA2_5DIGIT),
          .f = function(...) min(...)),
          shortest_time = duration(shortest_time, "seconds")) %>%
        select(SA2_5DIGIT, shortest_time)
      
      # Remove the previous shortest_time column, if it already exists
      if ("shortest_time" %in% colnames(drive.values$area_data)) {
        drive.values$area_data <- drive.values$area_data %>%
          select(-shortest_time)
      }
        
      # Add the new shortest dirving time column into the area data
      drive.values$area_data <- drive.values$area_data %>%
        left_join(shortest_time_new, by = "SA2_5DIGIT")
      
      # Get driving information for selected hospital
      selected_hospital <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(hospital_id)) %>%
        mutate(hospital = as.duration(.[[as.character(hospital_id)]])) %>%
        left_join(drive.values$area_data, by ='SA2_5DIGIT') %>%
        select(hospital, SA2_NAME, SA2_5DIGIT)
      
      # New row to add to clinic table
      new.row <- isolate(data.frame(
                 # Hospital Name
                 Hospital = input$hospital,
                 # Hospital ID
                 ID = hospital_id,
                 stringsAsFactors = FALSE))
      
      # Add the new row to the clinic table, if it wasnt already selected
      if ( !(input$hospital %in% isolate(drive.values$add_clinic_table$Hospital)) )
      { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row)) }
      
      # Display the new clinics table
      output$new.clinics.output <- renderUI({
        output$new.clinics.table <- renderTable(drive.values$add_clinic_table)
        tableOutput("new.clinics.table")
      })
      
      
    })
    
    ######################### When 'Add Clinic' Button is clicked in the filter selector ####################
    observeEvent(input$filter.add, {
      print(input$description.filter)
      print(input$sector.filter)
      print(input$beds.filter)
      
      htt.details.fitlered <- htt.details %>% { if( !(is.null(input$description.filter) ) ) filter(., Description %in% input$description.filter) else . } %>% 
                      { if( !(is.null(input$sector.filter) ) ) filter(., Sector %in% input$sector.filter) else . } %>%
                      { if( !(is.null(input$beds.filter) ) ) filter(., Beds %in% input$beds.filter) else . }
      
      drive.values$new_clinic_ids <- append(drive.values$new_clinic_ids, 
                                            htt.details.fitlered$Hospital_ID[!(htt.details.fitlered$Hospital_ID %in% 
                                                                               drive.values$new_clinic_ids)])
      
      # Use the new clinics id list to calculate the driving time to the nearest hospital
      shortest_time_new <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(drive.values$new_clinic_ids)) %>%
        mutate(shortest_time = pmap_dbl(
          .l = select(., -SA2_5DIGIT),
          .f = function(...) min(...)),
          shortest_time = duration(shortest_time, "seconds")) %>%
        select(SA2_5DIGIT, shortest_time)
      
      # Remove the previous shortest_time column, if it already exists
      if ("shortest_time" %in% colnames(drive.values$area_data)) {
        drive.values$area_data <- drive.values$area_data %>%
          select(-shortest_time)
      }
      
      # Add the new shortest dirving time column into the area data
      drive.values$area_data <- drive.values$area_data %>%
        left_join(shortest_time_new, by = "SA2_5DIGIT")
      
    })
    
    ######################### Build and display the selected clinics table ####################
    observeEvent({ input$location.add
                   input$filter.add }, {  
      # Get driving information for selected hospital
      selected_hospitals <- htt.nsw %>%
        select(SA2_5DIGIT, as.character(drive.values$new_clinic_ids)) %>%
        mutate(hospital = as.duration(.[[as.character(hospital_id)]])) %>%
        left_join(drive.values$area_data, by ='SA2_5DIGIT') %>%
        select(hospital, SA2_NAME, SA2_5DIGIT)
      
      # New row to add to clinic table
      new.row <- isolate(data.frame(
        # Hospital Name
        Hospital = input$hospital,
        # Hospital ID
        ID = hospital_id,
        stringsAsFactors = FALSE))
      
      # Add the new row to the clinic table, if it wasnt already selected
      if ( !(input$hospital %in% isolate(drive.values$add_clinic_table$Hospital)) )
      { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row)) }
      
      # Display the new clinics table
      output$new.clinics.output <- renderUI({
        output$new.clinics.table <- renderTable(drive.values$add_clinic_table)
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
      drive.values$new_clinic_ids <- c()
      
      # Reset the area data
      drive.values$area_data <- sa2.TB
    })
    
    ######################### UI for map customisation ###################################
    # map overlay selector when no clinics are chosen
    output$overlay.selector <- renderUI({
      
      # If no clinics have been selected
      if ( length(drive.values$add_clinic_table$Hospital) == 0) {
      selectInput("select.overlay", "Select area overlay:",
                  choices = c("Boundaries Only" = "blank.overlay",
                              "IRSD (Disadvantage)" = "irsd.overlay",
                              "Aboriginal and Torres Strait Islander Population" = "atsi.overlay"
                  ),
                  selected = c("Boundaries Only" = "blank.overlay"))
      
      } else {
        selectInput("select.overlay", "Select area overlay:",
                    choices = c("Boundaries Only" = "blank.overlay",
                                "IRSD (Disadvantage)" = "irsd.overlay",
                                "Aboriginal and Torres Strait Islander Population" = "atsi.overlay",
                                "Driving Time to Nearest Clinic" = "drive.overlay"
                    ),
                    selected = c("Boundaries Only" = "blank.overlay"))
      }
    
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
              actionButton("location.add", "Add Clinic")
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
    
    # Set colour palette for IRSD overlay
    pal.irsd <- eventReactive(input$drive.update, {
      pal <- colorFactor("YlOrRd", levels = seq(1,10,1), reverse = TRUE)
      pal
      
    })
    
    # Set colour palette for IRSD overlay
    pal.atsi <- eventReactive(input$drive.update, {
      pal <- colorNumeric("YlOrRd", domain = percent(drive.polys()$atsi))
      pal
      
    })
    
    # Set labels area mouse over
    labels.drive <- eventReactive(input$drive.update, {
        sprintf(
            "<strong>%s</strong><br/>",
            drive.polys()$NAME16
            ) %>% 
                lapply(htmltools::HTML)
    })
    
    ######################### Base Map ###############################
    # output the basic version of the map with elements that don't change
    output$drive.map <- renderLeaflet({ 
        leaflet() %>%
            addTiles() %>%
            setView(147.016667, -32.163333, zoom = 5.5) %>%
        # Add Polygons
        addPolygons(
          data = sa2.polys,
          layerId = ~CODE16,
          color = 'black',
          fillColor = 'white',
          weight = 1,
          opacity = 0.25,
          highlight = highlightOptions(
            weight = 3),
          label = sa2.polys$NAME16,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    })
    
    ######################### Observers for Map editting ###############
    
    # Adding polygons
    observeEvent(input$drive.update, {
        if (input$select.overlay == 'blank.overlay') {
          #Blank Overlay polygons
          leafletProxy("drive.map") %>% 
            # Clear old polygons
            clearShapes %>%
            # Add Polygons
            addPolygons(
              data = drive.polys(),
              layerId = ~CODE16,
              color = 'black',
              fillColor = 'white',
              weight = 1,
              opacity = 0.25,
              highlight = highlightOptions(
                weight = 3),
              label = drive.polys()$NAME16,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
            fitBounds(min(set_coords()$long),
                      min(set_coords()$lat),
                      max(set_coords()$long),
                      max(set_coords()$lat))
          
        } else if (input$select.overlay == 'drive.overlay') {
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
            
        } else if (input$select.overlay == 'irsd.overlay') {
          #IRSD Overlay polygons
          leafletProxy("drive.map") %>% 
            # Clear old polygons
            clearShapes %>%
            # Add new polygons
            addPolygons(
              data = drive.polys(),
              layerId = ~CODE16,
              fillColor = ~pal.irsd()(as.numeric(drive.polys()$IRSD)),
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
          
        } else if (input$select.overlay == 'atsi.overlay') {
          #ATSI Overlay polygons
          leafletProxy("drive.map") %>% 
            # Clear old polygons
            clearShapes %>%
            # Add new polygons
            addPolygons(
              data = drive.polys(),
              layerId = ~CODE16,
              fillColor = ~pal.atsi()(percent(drive.polys()$atsi)),
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
        if (input$select.overlay == 'blank.overlay') {
          # Remove legend for blank map
          leafletProxy("drive.map") %>%
            # Clear old legend 
            clearControls()
          
        } else if (input$select.overlay == 'drive.overlay') {
            
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
        
        } else if (input$select.overlay == 'irsd.overlay') {
         
          data <- drive.polys()@data %>% filter(!is.na(IRSD))
           # Legend for IRSD Map Overlay
          leafletProxy("drive.map") %>%
            # Clear old legend 
            clearControls() %>%
            # Add Legend
            addLegend(pal = pal.irsd(), 
                      values = data$IRSD, 
                      opacity = 0.7, 
                      title = "IRSD",
                      position = "bottomright",
                      layerId = 'drive.legend')
          
        } else if (input$select.overlay == 'atsi.overlay') {
            
            data <- drive.polys()@data %>% filter(!is.na(IRSD))
            # Legend for ATSI Map Overlay
            leafletProxy("drive.map") %>%
              # Clear old legend 
              clearControls() %>%
              # Add Legend
              addLegend(pal = pal.irsd(), 
                        values = data$IRSD, 
                        opacity = 0.7, 
                        title = "IRSD",
                        position = "bottomright",
                        layerId = 'drive.legend')
            
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
    
    #---------------RESET AREA SELECTION-----------#
    observeEvent(input$area.reset, {
      #clear area report
      output$area.output <- renderUI(div(style = 'overflow-y:scroll;height:580px;',
                                         box(title = "Select New Areas",
                                             width = 12)))
      
      #reset the map clicks
      drive.values$map.clicks <- c()
      
    })
    
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
