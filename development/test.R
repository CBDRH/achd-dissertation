
library(shiny)
library(rgdal)
library(sp)
library(plyr)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

print(getwd())
setwd("/Users/calumnicholson/Documents/OneDrive - UNSW/Disseration/data")

csv <- read.csv('ASGS/asgs2016codingindexes/2017_Locality_to_2016_SA2_Coding_Index.csv')
polys <- readOGR('ASGS/1270055001_sa2_2016_aust_shape/SA2_2016_AUST.shp')

NSW_poly <- subset(polys, polys$GCC_NAME16 %in% c("Rest of NSW", "Greater Sydney"))
simple_poly <- rgeos::gSimplify(NSW_poly, tol = 0.01)

# test the polygon in a map
leaflet() %>%
  addTiles %>%
  addPolylines(data = NSW_poly, weight = 1)
