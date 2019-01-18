setwd("~/NSSdatascience/R/Heart_Disease_Mortality_Fast_Food")


library(tidyverse)
#library(plotly)
#library(tidyr)
#library(ggmap)
#library(dplyr)
#require(scales)
#library(ggpubr)
#library(directlabels)
#library(qcc)
#library(magrittr)
library(maps)
library(maptools)
library(tmap)
#library(raster)
library(tmaptools)
library(leaflet)
#library(readxl)
library(sf)



# FFR <- readRDS(file = './data/FFR.RDS')
# county_cvd_2017 <- readRDS(file = './data/county_cvd_2017.RDS')
# FFR_shape <- st_read("./data/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp")
# FFR_shape <- append_data(FFR_shape, county_cvd_2017, key.shp = "GEOID", key.data = "GEOID",ignore.duplicates = TRUE)
# FFR_shape_merge <- append_data(FFR_shape, FFR, key.shp = "GEOID", key.data = "GEOID",ignore.na = TRUE)
# FFR_shape_merge <- FFR_shape_merge[!(FFR_shape_merge$State %in% c("AK", 'HI',"DC", "PR")),]
# 
# FFR_shape_merge <- FFR_shape_merge %>%
#   dplyr::select(-un_2012,-unnsrd_,-County_name,-NAME.data, -State.data,-State_name.data, -NAME.data.1) %>%
#   dplyr::rename("Per_Rec_Fac_09-14" = "PCH_RECFACPTH_09_14", "Adult_Obese_Per_13" = "PCT_OBESE_ADULTS13",
#          "Adults_Diabetes_per_13" = "PCT_DIABETES_ADULTS13", "Per_65_Older_10"="PCT_65OLDER10")
# saveRDS(FFR_shape_merge, file = "FFR_shape_merge.RDS")
FFR_shape_merge <- readRDS(file = './data/FFR_shape_merge.RDS')
US_states <-FFR_shape_merge %>% aggregate_map(by = "State")

var <- "Age_Adjusted_Rate"
#cuts <- c(75,100, 175,225,300,375,475)
#cuts <- c(0, 10, 20, 30, 40, 100)
mycols <- c("#90e0dc", "#287c28", "#000033","#bebebe")


mycols2 <- c("#90e0dc", "#287c28","#bdf8d1","#50c878", "#2f4558","#dcf4f0","#50c8b4")
  #cuts <- c(0.1, 0.3, 0.5, 1,3,4,6)
  
tm <- tm_shape(FFR_shape_merge, projection = 2163) +
    tm_fill(var,
              midpoint = 0,
            n=9,
              palette = "Set1",
              border.col = "black",
              title='Age Adjusted Rate',
              border.alpha = .5,
              id = "NAME",
              textNA = 'Unreliable',
              colorNA = "grey",
            popup.vars = c("Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
               "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
               "Number of FastFood Resturants 09: " = "FFR09",
               "Number of FastFood Resturants 14: " = "FFR14"))+
  tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
  
 
   tm_bubbles(size="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
             midpoint = NA,
             title.size="FFR % change 09 to 14",
             title.col = "FFR % change 09 to 14",
             palette = mycols,
             popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                            "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                            "Number of FastFood Resturants 09: " = "FFR09",
                            "Number of FastFood Resturants 14: " = "FFR14"))+
    tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .25) +
  tm_style("natural")+
  tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
            title.size = 1.1,
            title.position = c("center", "top"),bg.color = "gray95")+
   tm_legend(legend.position = c("right", "bottom"))

tmap_leaflet(tm)

# STATE SPECIFIC MAP

tn <- FFR_shape_merge %>% filter(State == 'TN')

var <- "Age_Adjusted_Rate"
#cuts1 <- c(100,120,180,200,300,350,400,450)
tm1 <- tm_shape(tn, projection = 2163) +
  tm_fill(var,
          midpoint = 0,
          n=9,
          palette = "Set1",
          border.col = "white",
          title='Age Adjusted Rate',
          border.alpha = 0.5,
          id = "NAME",
          textNA = 'Unreliable',
          colorNA = "grey",
          popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                             "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                             "Number of FastFood Resturants 09: " = "FFR09",
                             "Number of FastFood Resturants 14: " = "FFR14",
                             "Number of FastFood Resturants 14: " = "FFR14"))+
  
  tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
 
  tm_bubbles(size ="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
             palette = mycols,
             midpoint = NA,
             title.size="FFR % change 09 to 14",
             title.col = "FFR % change 09 to 14",
             popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                            "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                            "Number of FastFood Resturants 09: " = "FFR09",
                            "Number of FastFood Resturants 14: " = "FFR14"))+
  tmap_options(max.categories = 14)+
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .25) +
  tm_style("natural")+
  tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
            title.size = 1.1,
            title.position = c("center", "top"))+
  tm_legend(legend.position = c("right", "bottom"))
  
tmap_leaflet(tm1) 


