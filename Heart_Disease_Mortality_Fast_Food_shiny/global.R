library(shiny)
library(shinythemes)
library(tidyverse)
library(maps)
library(maptools)
library(tmap)
library(raster)
library(tmaptools)
library(leaflet)
library(sf)
library(magrittr)
library(dplyr)
library(shinycustomloader)
library(shinycssloaders)

FFR_shape <- readRDS(file = './data/FFR_shape_merge.RDS')
US_states <-FFR_shape %>% aggregate_map(by = "State")


states <- as.data.frame(FFR_shape) %>% 
 dplyr::select(State_name) %>% 
  unique()

states <- sort(states$State_name)

#name <- c('Age_Adjusted_Rate',"Percent_Adults_Diabetes_2013",
          #"Percent_Adults_Obese_2013","Rec_Facilities_per_1000_09_14","PCT_65OLDER10")


mycols <- c("#90e0dc", "#287c28", "#000033","#bebebe")
mycols2 <- c("#F781BF","#E41A1C", "#377EB8" ,"#4DAF4A", "#984EA3", "#FF7F00" ,"#FFFF33", "#A65628","#CB181D")


