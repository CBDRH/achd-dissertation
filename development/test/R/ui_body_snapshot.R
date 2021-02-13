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
        fluidRow(
          valueBox(length(achd$achd_id), 
                   "Number of Patients", 
                   width = 3, color = 'light-blue'),
          valueBox(achd %>% filter(bethesda_code == 1) %>% nrow(), 
                   "Patients with Simple CHD", 
                   width = 3, color = 'light-blue'),
          valueBox(achd %>% filter(bethesda_code == 2) %>% nrow(), 
                   "Patients with Moderate CHD", 
                   width = 3, color = 'light-blue'),
          valueBox(achd %>% filter(bethesda_code == 3) %>% nrow(), 
                   "Patients with Complex CHD", 
                   width = 3, color = 'light-blue')
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
)