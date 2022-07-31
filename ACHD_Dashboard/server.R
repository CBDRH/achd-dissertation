
# Define server logic
function(input, output) {    
    # Function to search for input names
    getInputs <- function(pattern){
        reactives <- names(reactiveValuesToList(input))
        reactives[grep(pattern,reactives)]
    }
    
    ############################# GLOBAL REACTIVE FUNCTIONS #################################
    
    # Global Filters - not active in public version
    # achd.filtered <- 
    
    # count frequency of diagnoses - not active in public version
    # dx.count <- 
    
    # prepare area level data
    drive.area.achd <- reactive({
        
        # Join achd area data to table builer data
        area.drive <- left_join(sa2.TB, achd, by = 'SA2_NAME') %>%
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
    
    observe(
        
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
        { isolate(drive.values$add_clinic_table <- rbind(drive.values$add_clinic_table, new.row))}
        
        # Display the new clinics table
        output$new.clinics.output <- renderUI({
            output$new.clinics.table <- renderTable(drive.values$add_clinic_table %>%
                                                        select(-ID) %>% # Do not need to display the Hospital ID number
                                                        mutate(Patients = mask.achd(Patients, 'str'), #mask value for low
                                                               ltf = mask.achd(ltf, 'str'))
            ) 
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
            valueBox(tags$p(sum(drive.area.achd()$ACHD_count),
                            style = "font-size: 50%;"),
                     'Selected Patients', 
                     width = 3, color = 'light-blue')
        })
        # Breakdown is disease severity
        output$beth.count.drive <- renderValueBox({
            valueBox(tags$p(
                paste(sum(drive.area.achd()$beth_1), " | ",
                      sum(drive.area.achd()$beth_2), " | ",
                      sum(drive.area.achd()$beth_3), 
                      sep = "  "),
                style = "font-size: 50%;"),
                'Simple | Moderate | Severe',
                color = 'light-blue'
            )
        })
        # Total Number of Patients Lost to Follow up
        output$ltf.count.drive <- renderValueBox({
            valueBox(tags$p(sum(drive.area.achd()$ltf_3),
                            style = "font-size: 50%;"), 
                     "Patients Lost to Follow up", 
                     color = 'light-blue')
        })
        # Total Number of Patients within 1 hour drive
        output$hr.count.drive <- renderValueBox({
            valueBox(tags$p(
                drive.values$area_data %>% filter(as.numeric(shortest_time, "hours") <= 1) %>%
                    summarise(hr_drive = sum(ACHD_count)) %>%
                    pull(hr_drive),
                style = "font-size: 50%;"),
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
            %s ACHD patients<br/>
            Simple: %s | Moderate: %s | Severe: %s<br/>
            %.1f hr drive to nearest clinic<br/>",
            drive.polys()$NAME16, 
            mask.achd(drive.polys()$ACHD_count,'str'),
            mask.achd(drive.polys()$beth_1, 'str'), mask.achd(drive.polys()$beth_2, 'str'), mask.achd(drive.polys()$beth_3, 'str'),
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
      
        showModal(modalDialog(
            title = NULL,
            uiOutput('area.output'),
            uiOutput('area.dx.box'), # Diagnoses Present in selected area
            footer = modalButton("Done"),
            easyClose = TRUE,
            fade = TRUE
            ))  
        
    })
    
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
                h2(HTML(paste("Area Focus", icon('map')))),
                hr(),
                    div(style="display:inline-block",actionButton('area.button', 'Add to report and close')),
                    div(style="display:inline-block",actionButton("area.reset", "Cancel")),
                    HTML(paste(
                        "
            <br><br>
            <b>Selected Areas:</b> ", paste(drive.values$event_area$SA2_NAME, collapse = ', '), "<br>
            <br>
            <b>Number of ACHD Patients:</b> ", mask.achd(sum(drive.values$event_area$ACHD_count), 'str'),"<br>
            <br>
            <b>Number with:</b> <br>
            <ul>
              <li>Simple CHD: ",mask.achd(sum(drive.values$event_area$beth_1), 'str'),"</li>
              <li>Moderate CHD: ",mask.achd(sum(drive.values$event_area$beth_2), 'str'),"</li> 
              <li>Complex CHD: ",mask.achd(sum(drive.values$event_area$beth_3), 'str'),"</li>
            </ul>
            <b>Number of Patients lost to follow up:</b><br> 
            <ul>
              <li>",mask.achd(sum(drive.values$event_area$ltf_3), 'str'),"</li>
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
                filter(dx_count > 0) %>%
                mutate(dx_count = if_else(dx_count<5, 5, dx_count))
            
            ggplot(data = dx.area.data, aes(x = reorder(dx_label, dx_count), y=dx_count)) + 
                geom_bar(stat="identity") +
                scale_y_continuous(breaks = seq(0, max(dx.area.data$dx_count), 1)) +
                theme(axis.text.x = element_text(angle = 90)) +
                labs(title = "For privacy, counts of less than 5 have been displayed as 5", 
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
        
        removeModal()
        
        shinyalert("Success!", "Added to report.", type = "success", timer=1000, showConfirmButton = FALSE)
        
    })
    
    #---------------RESET AREA SELECTION-----------#
    observeEvent(input$area.reset, {
        # Clear the area dx plot
        output$area.dx.box <- renderUI({
            box(title = "Select New Areas")
        })
        
        #reset the map clicks
        drive.values$map.clicks <- c()
        
    removeModal()
    
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
            
            shinyalert("Success!", "Compiling report.", type = "info", timer=2000, showConfirmButton = FALSE)
            # showModal(modalDialog(
            #     title = "Download Complete",
            #     easyClose = TRUE,
            #     footer = NULL))
            
            rmarkdown::render(tempReport, output_file = file)
            
        }
        
    )
    
}



# ############################# RUN APP #################################
# 
# # Run the application 
# shinyApp(ui = ui, server = server)