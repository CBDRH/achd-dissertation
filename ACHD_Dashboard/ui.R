#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$head(tags$style(HTML('.nav-tabs-custom .nav-tabs li.active {margin-left: -20px;}'
                            )),
                  tags$style(HTML('.shiny-table {margin-left: -25px;}'
                            )),
                  tags$style(HTML('.small-box.bg-light-blue {margin-left: 0px;
                                                             margin-right: 0px;
                                                             border: 0px}'
                  )),
                  ),
        navbarPage(
    
    "Clinic Planning Tool",
                   id = "panel", 
                   selected = 'home',

                   tabPanel("", value = "home", icon = icon('home'),
                            
                            includeCSS('www/custom.css'), # Include css
                            tippy::tippy_class("tool-tip"), # Need this for tippy libraries to be loaded
                            
                            sidebarLayout(
                                
                                sidebarPanel(width=2, height=900, style="background-color: white",
                                             
                                    # Report Download Button
                                    h4("Download Report"),
                                    div(style="display:inline-block",
                                        downloadButton("html.download", "HTML Report", icon = NULL,
                                                       style = "height: 50px; width: 75%; font-size: 14px; 
                                                                float:centre; margin: 0px 0px 0px 15px; color: #fff; 
                                                                background-color: #27ae60; border-color: #fff")), # HTML Download
                                    div(style="display:inline-block",
                                        downloadButton("pdf.download", "PDF Report",  icon = NULL,
                                                       style = "height: 50px; width: 75%; font-size: 14px; 
                                                                float:centre; color: #fff; 
                                                                background-color: #27ae60; border-color: #fff")), # PDF Download
                                    br(),     
                                    hr(),     
                                    h4(HTML(paste("Global Filters", icon('sliders-h')))), # Title
                                    hr(),
                                    div(style="display:inline-block",actionButton("sb.update", "Apply")), # Apply Filters button
                                    div(style="display:inline-block",actionButton("sb.reset", "Reset")), # Reset Filters button
                                    br(),
                                    uiOutput("out.bethesda"), # filter by disease severity
                                    uiOutput("out.sex"), # fitler by sex
                                    uiOutput("out.mortality"), # filter my mortality
                                    uiOutput("out.age"), # filter by age
                                    uiOutput("out.dates"), # filter by time range
                                    uiOutput("out.last.clinic"), # filter by time since last clinic visit
                                    hr()
                                    
                                ),
                                
                                
                                mainPanel(width=10,
                                          fluidPage(
                                              column(width = 8,
                                                     leafletOutput('drive.map', height = 660),
                                                     
                                                     # Summary Value Boxes
                                                     valueBoxOutput('pt.count.drive', width = 3), # Total Number of Patients
                                                     valueBoxOutput('beth.count.drive', width = 3), # Breakdown of disease Severity
                                                     valueBoxOutput('ltf.count.drive', width = 3), # Number of Patients lost to follow up
                                                     valueBoxOutput('hr.count.drive', width = 3), # Number of patients within a 1 hour drive
                                                     
                                                     
                                                     
                                                     
                                                        ), 

                                              column(width=4,
                                                     
                                                     # Map Customisation
                                                     box(collapsible = T, collapsed = F, title = tippy(HTML(paste('Map Customisation', icon('info'))), 
                                                                       "<p class='helpBox'><b>Map Customisation</b><br>
                                                                        The first option ‘Filter by Region’ allows you to specifically choose geographical areas based on larger region, 
                                                                        by default all regions are selected. Deselecting highly populated areas (‘Greater Sydney’ and ‘ACT’) can help to 
                                                                        explore less populated areas (‘Rest of NSW’) in more details. There are two overlay options that can be places on the map. The first ‘Driving Time to Nearest Clinic’ will 
                                                                        display a choropleth of the driving time from each area to the nearest ACHD Clinics. The second, ‘Total ACHD 
                                                                        Population’ will display a choropleth of the number of ACHD patients in each area.<br><br>
                                                                       <b>Clicking the ‘Update’ button will load the app and add any updates or changes that you have selected.</b><br>
                                                                       </p>"
                                                                       ), 
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
                                                         width = 12 
                                                     ),
                                                    
                                                    # Information about Hospital Coverage
                                                     box(collapsible = T, collapsed = F, title = tippy(HTML(paste('Clinic Tables', icon('info'))),
                                                                        "<p class = 'helpBox'><b>Clinic Tables</b><br>
                                                                        There are two tabs in this section, “Current Clinics” provides information about the current ACHD Clinics and 
                                                                        “New Clinics” provide information about the newly selected clinics.<br>
                                                                       </p>"
                                                     ),       
                                                     tabBox(title = NULL,
                                                            id = "clinic.tabs", 
                                                            tabPanel("Current\nClinics", uiOutput("current.clinic.output")), # Current hospitals
                                                            tabPanel("New\nClinics", uiOutput("new.clinics.output")), # Newly Selected Hospitals
                                                            width = 12),
                                                     width=12),
                                                    
                                                    # Selecting New Clinics
                                                    box(collapsible = T, collapsed = T, title = tippy(HTML(paste('New Clinic Selection', icon('info'))),
                                                                                                      "<p class = 'helpBox'><b>New Clinic Selection</b><br>
                                                                        This section will allow you to choose hospitals for new clinic locations. First select the desire Primary 
                                                                        Health Network Area, this will allow you to choose the Local Hospital Network area which will in turn allow 
                                                                        you to select a hospital from that area. Click “Add Clinic” to add the new hospital to the list and “Reset 
                                                                        Clinics” to remove all the selected hospitals. Once you have chosen all the desired hospital locations, click 
                                                                        ‘update’ in the Map Customisation box to add the clinics to the map.<br>

                                                                       </p>"
                                                    ),                                       
                                                    uiOutput('phn.selector'), # Primary Health Network
                                                    uiOutput('lhn.selector'), # Local Health Network
                                                    uiOutput('hospital.selector'), # Hospital
                                                    div(style="display:inline-block",uiOutput('clinic.button.add')), # Add hospital to list
                                                    div(style="display:inline-block",uiOutput('clinic.button.reset')), # Reset list
                                                    width = 12
                                                    )
                                                     
                                                     ) # Closes right hand column
                                          ) # Closes fluidPage
                                ) # Closes main panel
                            ) # Closes sidebarLayout
                        ), # Closes Home panel
                   
                    tabPanel("", value = 'snapshot', icon = icon('chart-column'),
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
                    ), # Closes snapshot panel
    
                # Info panel
                   tabPanel("", value = 'info', icon = icon('info-circle'),
                            
                        column(width=6,
                            
                            box(title = "Introduction", width=12,
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
                                    ))
                                   ),
    
    
                        column(width=6,
                               
                       box(title = "Data Sources", width=12, collapsible = T, collapsed = T,
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
                           )),       
                               
                               
                        box(title = "Key Definitions", width=12, collapsible = T, collapsed = T,
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
                            )),
                        
                        
                        box(title = "References", width=12, collapsible = T, collapsed = T,
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
                            ))
                        )
                            
                            
                    ) # Closes info panel
                   

)))
