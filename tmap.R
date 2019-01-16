library(tidyverse)
#library(plotly)
#library(tidyr)
#library(ggmap)
#library(dplyr)
#require(scales)
library(ggpubr)
library(directlabels)
#library(qcc)
library(magrittr)
library(maps)
library(maptools)
library(tmap)
library(raster)
library(tmaptools)
library(leaflet)
#library(readxl)
library(sf)



# FFR <- readRDS(file = './Midstone_Project/Midstone/FFR.RDS')
# county_cvd_2017 <- readRDS(file = './Midstone_Project/Midstone/county_cvd_2017.RDS')
# FFR_shape <- shapefile("./Midstone_Project/Midstone/gz_2010_us_050_00_20m/gz_2010_us_050_00_20m.shp")
# 
# FFR_shape <- FFR_shape[!(FFR_shape$STATE %in% c("02","15","72")),] 
# FFR_shape$FIPS <- paste0(FFR_shape$STATE, FFR_shape$COUNTY)
# FFR_shape$FIPS <- as.numeric(FFR_shape$FIPS)
# FFR_shape <- append_data(FFR_shape, FFR, key.shp = "FIPS", key.data = "FIPS",ignore.duplicates = TRUE)
# 
# FFR_shape <- append_data(FFR_shape, county_cvd_2017, key.shp = "FIPS", key.data = "FIPS",ignore.duplicates = TRUE)
# # unmatched_data <- over_coverage()
# # str(unmatched_data)
# US_states <-FFR_shape %>% aggregate_map(by = "STATE")
# cuts <- c(0,50,100,150,200,250,300,350,400,450,475)
# #cuts <- c(,0.1, 0.3, 0.5, 1,3,4,6)
#   tm <- tm_shape(FFR_shape, projection = 2163)+
#   tm_polygons("Age.Adjusted.Rate", breaks = cuts,border.col ="black", title="Age.Adjusted.Rate", 
#               palette = 'BuPu')+
#   tm_bubbles(size ="FFRPTH14", col = "FFRPTH14",
#              popup.vars = c('Age.Adjusted.Rate','FFRPTH14','County.data','State_name'),
#   palette = "-RdYlGn",
#   breaks = c(0,0.1, 0.3, 0.5, 1,3,4,6))+
#   tm_shape(US_states) +
#   tm_borders(lwd=2, col = "black", alpha = .25) +
#   tm_layout(title="FastFood restaurants/1000 persons per county", 
#             title.position = c("center", "top"),
#             legend.text.size=1)+
#   tm_view(view.legend.position = c("left", "bottom"))+
#   tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08))
#   tmap_leaflet(tm)
#   
# #tmap_mode("view")
# 
#   # trial
#   
# head(FFR_shape)
  
#FFR <- read_csv("./Midstone_Project/data/FFR_BACKUP.csv")
# State_abbrev <- read.csv('./Midstone_Project/data/50_us_states_all_data.csv')
# FFR <- merge(FFR, State_abbrev, by = 'State')
# write.csv(as.data.frame(FFR), file = './Midstone_Project/data/FFR_State_abbrev_merge.csv')
#FFR <- FFR %>% dplyr::select(FIPS, State, County, FFRPTH14)
#FFR <- readRDS(file = './Midstone_Project/Midstone/FFR.RDS')
#FFR <- FFR %>% rename(GEOID = 'FIPS')
# county_cvd_2017 <- read_excel("./Midstone_Project/data/2017_cvd_deaths.xls", na = 'Unreliable')
# county_cvd_2017 <- county_cvd_2017 %>% mutate(Age_Adjusted_Rate = as.numeric(Age_Adjusted_Rate))
# saveRDS(county_cvd_2017, file = "county_cvd_2017.RDS")
#saveRDS(FFR, file = "FFR.RDS")
# county_cvd_2017$FIPS <- sprintf('%01d',county_cvd_2017$FIPS)
# county_cvd_2017 <- county_cvd_2017 %>% rename(GEOID = 'FIPS')
#FFR_merge <- left_join(county_cvd_2017, FFR, by = 'GEOID')

# sum(is.na(FFR$PCH_FFRPTH_09_14))
# sum(is.na(county_cvd_2017$Age_Adjusted_Rate))

# FFR <- readRDS(file = './Midstone_Project/Midstone/FFR.RDS')
# county_cvd_2017 <- readRDS(file = './Midstone_Project/Midstone/county_cvd_2017.RDS')
# FFR_shape <- st_read("./Midstone_Project/acs_2012_2016_county_us_B27001/acs_2012_2016_county_us_B27001.shp")
# FFR_shape <- append_data(FFR_shape, county_cvd_2017, key.shp = "GEOID", key.data = "GEOID",ignore.duplicates = TRUE)
# FFR_shape_merge <- append_data(FFR_shape, FFR, key.shp = "GEOID", key.data = "GEOID",ignore.na = TRUE)
# FFR_shape_merge <- FFR_shape_merge[!(FFR_shape_merge$State %in% c("AK", 'HI',"DC", "PR")),]
# saveRDS(FFR_shape_merge, file = "FFR_shape_merge.RDS")
FFR_shape_merge <- readRDS(file = './Heart_Disease_Mortality_Fast_Food/data/FFR_shape_merge.RDS')
shpsimp <- simplify_shape(FFR_shape_merge, fact = 0.05)
US_states <-FFR_shape_merge %>% aggregate_map(by = "State")

# %>%
#
# unmatched_data <- over_coverage()
# str(unmatched_data)
# View(FFR_shape[unmatched_data$id, ])
var <- "Age_Adjusted_Rate"
cuts <- c(75,100, 175,225,300,375,475)
#cuts <- c(0, 10, 20, 30, 40, 100)
#mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")
  #cuts <- c(0.1, 0.3, 0.5, 1,3,4,6)
  
tm <- tm_shape(FFR_shape_merge, projection = 2163) +
    tm_fill(var,
              breaks = cuts,
              palette = "BuPu",
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
             breaks = c(-100, -75, -50, -25, -10,0, 10,25,50,100, 200,300),
             title.size="FFR % change 09 to 14",
             title.col = "FFR % change 09 to 14",
             palette = "-RdYlGn",
             popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                            "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                            "Number of FastFood Resturants 09: " = "FFR09",
                            "Number of FastFood Resturants 14: " = "FFR14"))+
    tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .25) +
  tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
            title.size = 1.1,
            title.position = c("center", "top"),bg.color = "gray95")+
   tm_legend(legend.position = c("right", "bottom"))+
tm_style("natural")
tmap_leaflet(tm)




# shp <-  FFR_shape_merge %>%
#   mutate(STFIPS = stringr::str_sub(GEOID, 1, 2))


# %>%
#   mutate(NAME = stringr::str_remove(NAME, ", Tennessee"))
tn <- FFR_shape_merge %>% filter(State == 'TN')

var1 <- "Age_Adjusted_Rate"
cuts1 <- c(100,120,180,200,300,350,400,450)
tm1 <- tm_shape(tn, projection = 2163) +
  tm_fill(var1,
              breaks = cuts1,
              palette = "BuPu",
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
  
  tm_polygons(col = var1, alpha = 0.5, border.col = 'white')+
 
  tm_bubbles(size ="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
             title.size="FFR % change 09 to 14",
             title.col = "FFR % change 09 to 14",
             popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                            "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                            "Number of FastFood Resturants 09: " = "FFR09",
                            "Number of FastFood Resturants 14: " = "FFR14"),
             palette = "-RdYlGn",
             breaks = c(-100, -75, -50, -25, -10,0, 10,25,50,100, 200,300))+
  tmap_options(max.categories = 14)+
  tm_shape(US_states) +
  tm_borders(lwd=2, col = "black", alpha = .25) +
  tm_layout(title = "Heart Disease Mortality and Fast Food Resturants",
            title.size = 1.1,
            title.position = c("center", "top"))+
  tm_legend(legend.position = c("right", "bottom"))+
  tm_style("natural")
tmap_leaflet(tm1) 

