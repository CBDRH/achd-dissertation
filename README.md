# An Interactive Geographic Information System to Inform Optimal Locations for Healthcare Services

Code for the preparation of data from adult congenital heart disease services in New South Wales, and the presentation of this data in a Geographic Information System

## Contents

### Analysis Pipeline

The Analysis Pipeline is designed to prepare all the data for the Shiny App in the "ACHD_Dashboard" folder. The input files are provided and the analysis pipeline will take those files to create the data neccesary for the App the funciton. This pipeline must be run before using the Shiny App.

-    **00_Run_Analysis_Pipeline.R** - an R script that will run all of the following .RMD files and save their outputs in the outputs folder, it will also copy the outputs folder into the "ACHD_Dashboard" folder, for use by the Shiny App. Only this file need to be run to trigger all the files in the Analysis Pipeline.
-    **01_ASGS_Lookup.Rmd** - Takes the coding tables from the Australian Statistical Geographic Standard (2016 version) from the Australian Bureau of Statistics and prepares this data for use in the Shiny App.
-    **02_Hosp_Travel_time.Rmd** - Prepared the data provided by "Barbieri S, Jorm L. Travel times to hospitals in Australia. Sci Data. 2019;6: 248." for use in the Shiny App.
-    **04_ASGS_Boundaries.Rmd** - Prepares the shape files from the Australian Statistical Geographic Standard (2016 version) from the Australian Bureau of Statistics and prepares this data for use in the Shiny App.
-    **05_ACHD_database.Rmd** - Prepares the patient data from the Adult Congential Heart Disease Service at Royal Prince Alfred Hospital for use in the Shiny App. Notes that this is a protected patient dataset that is not available in the GitHub and can only be access onsite at Royal Prince Alfred Hospital. The Public branch of this repository provide an aggregated version of the data for use, and in that branch the "00_Run_Analysis_Pipeline.R" script does not run this file.

#### Inputs

-    **`ASGS`** - A folder containing shape files from the Australian Statistical Geographic Standard (2016 version) from the Australian Bureau of Statistics, each subfolder contains the files for different boundary levels, including `SA2`, `SA3` and `SA4`
-    **`Table_Builder_SA2`** - A folder containing demographic data for NSW, at the level of Statistical Area 2 from the 2016 census, downloaded from Table Builder
-    **`2019 Locality to 2016 SA2 Coding Index.csv`** - A match up table to convert a Suburb + Postcode combination to Statisical Area 2
-    **`ACHD_EPCC_coding.csv`** - A match up between diagnoses categories in the Adult Congenital Heart Disease Database and the European Paediatric Congenital Cardiology Code - Short List
-    **`duration_sa2_hospitals.csv`** - Travel times from Statistical Area 2 to each hospital in Australia, provided by "Barbieri S, Jorm L. Travel times to hospitals in Australia. Sci Data. 2019;6: 248."
-    **`myhospitals-contact-details.csv`** - Metadata about hospitals in Australia, provided by "Barbieri S, Jorm L. Travel times to hospitals in Australia. Sci Data. 2019;6: 248."
-    **`SA2_2016_AUST.csv`** - A match up table between different levels of the Australian Statistical Geographic Standard (2016 version) 


### ACHD Dashboard

This folder contains the shiny app that makes up the Geogrpahic Information System for this project

#### Contents

-    **Global.R** - The Header information for the shiny app
-    **ui.R** - The UI side of the shiny app
-    **Server.R** - The server side of the shiny app
-    **Report.Rmd** - The master document for the downloadable report in the shiny app
-    **New_Clinic_Report.Rmd** - populates the New Clinic section of the downloadable report
-    **Area_Report.Rmd** - populates the Area section of the downloadable report
-    **data** - A folder created and populated by the analysis pipeline for the data used in the shiny app












