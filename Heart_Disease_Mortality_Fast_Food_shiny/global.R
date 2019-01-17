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

FFR_shape_merge <- readRDS(file = './data/FFR_shape_merge.RDS')
US_states <-FFR_shape_merge %>% aggregate_map(by = "State")


states <- as.data.frame(FFR_shape_merge) %>% 
  dplyr::select(State_name) %>% 
  unique()

states <- sort(states$State_name)
mycols <- c("#90e0dc", "#287c28", "#000033","#bebebe")
