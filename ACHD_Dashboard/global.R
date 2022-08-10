# Libraries

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
library(tippy)
library(shinyalert)

# Path to ACHD database data (note: only accessible at RPAH)
pt_data <- 'Z:/CURRENT_STUDIES/2020_achd_map/DATA/'

################################## LOAD DATA ############################################

# import ACHD data
achd <- readRDS(file = paste(pt_data, 'output/rpah_analysis_dataset.rds', sep=""))

#import DX coding
dx_codes <- read.csv(file = "./data/ACHD_EPCC_coding.csv", fileEncoding="UTF-8-BOM") %>% 
  filter(!(Variable == 'adm_AbsentPA' | Variable == 'PA' | Variable == "achd_id")) %>%
  select(!c(ACHD_Database_name, check., Variable))

#import Census data
sa2.TB <- readRDS(file = "./data/sa2_demographics.rds") %>%
  mutate(IRSD = ordered(IRSD, c(1,2,3,4,5,6,7,8,9,10)),
         IRSAD = ordered(IRSAD, c(1,2,3,4,5,6,7,8,9,10)))

#import htt data
htt.nsw <- readRDS(file = "./data/htt_nsw.rds") # Driving times data
htt.details <- readRDS(file = "./data/htt_details.rds") # Hospital Metadata

#current achd clinics
achd_ids <- c(152, 408, 646, 683, 737, 979)
#placeholer for adding new clinics
new_achd_ids <- achd_ids

#Import area polygons
sa2.polys <- readOGR("./data/ASGS/sa2/sa2_polys.shp")
