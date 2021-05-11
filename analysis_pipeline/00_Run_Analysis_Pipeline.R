library(here)

# Make a folder for the analysis pipeline output
dir.create(here('analysis_pipeline','output'))

# Run each step of the analysis pipeline

rmarkdown::render(here('analysis_pipeline',"01_ASGS_Lookup.Rmd"))
rmarkdown::render(here('analysis_pipeline',"02_Hosp_Travel_Time.Rmd"))
rmarkdown::render(here('analysis_pipeline',"03_SA2_Demographics.Rmd"))
rmarkdown::render(here('analysis_pipeline',"04_ASGS_Boundaries.Rmd"))


# Copy the required analysis pipeline outputs into the Shiny App data folder
dir.create(here('ACHD_Dashboard', 'data'))

# SA2 area demographics
file.copy(here("analysis_pipeline", "output", "sa2_demographics.rds"),
          here("ACHD_Dashboard", "data", "sa2_demographics.rds"),
          overwrite = FALSE
)

# Driving time to hospitals
file.copy(here("analysis_pipeline", "output", "htt_nsw.rds"),
          here("ACHD_Dashboard", "data", "htt_nsw.rds"),
          overwrite = FALSE
)

# Hospital Metadata
file.copy(here("analysis_pipeline", "output", "htt_details.rds"),
          here("ACHD_Dashboard", "data", "htt_details.rds"),
          overwrite = FALSE
)

# Copy the shape files
dir.create(here("ACHD_Dashboard", "data", "ASGS"))
file.copy(
  here("analysis_pipeline", "output", "ASGS"),
  here("ACHD_Dashboard", "data"),
  overwrite = FALSE,
  recursive = TRUE
)

