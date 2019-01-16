
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(ggpubr)
library(directlabels)
library(magrittr)
library(maps)
library(maptools)
library(tmap)
library(raster)
library(tmaptools)
library(leaflet)
library(sf)

# FFR <- readRDS(file = './Midstone_Project/Midstone/FFR.RDS')
# county_cvd_2017 <- readRDS(file = './Midstone_Project/Midstone/county_cvd_2017.RDS')
#FFR_shape <- st_read("./Midstone_Project/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp")
# FFR_shape <- append_data(FFR_shape, county_cvd_2017, key.shp = "GEOID", key.data = "GEOID",ignore.duplicates = TRUE)
# FFR_shape_merge <- append_data(FFR_shape, FFR, key.shp = "GEOID", key.data = "GEOID",ignore.na = TRUE)
# FFR_shape_merge <- FFR_shape_merge[!(FFR_shape_merge$State %in% c("AK", 'HI', "PR")),]
FFR_shape_merge <- readRDS(file = './data/FFR_shape_merge.RDS')
US_states <-FFR_shape_merge %>% aggregate_map(by = "State")

# Tennessee map

# shp <-  FFR_shape_merge %>%
#   mutate(STFIPS = stringr::str_sub(GEOID, 1, 2))


#tn <- filter(FFR_shape_merge, State_name == 'Tennessee') 
# %>%  mutate(NAME = stringr::str_remove(NAME, ", Tennessee"))

states <- as.data.frame(FFR_shape_merge) %>% 
  dplyr::select(State_name) %>% 
  unique()

states <- sort(states$State_name)
