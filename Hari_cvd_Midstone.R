library(tidyr)
library(tidyverse)
library(ggpubr)
library(plotly)
library(R.utils)
library(janitor)
library(urbnmapr)
library(ggcorrplot)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)


#HD : cardiovascular disease

HD_prevalence_2013_2017 <- read.csv('./Midstone_Project/data/Cardiovascular_prevalence_2013-2017_kff.csv')
Top_15_deaths <- read.csv('./Midstone_Project/data/Top_15_causes_death_1999-2017.csv')
HD_deaths_gender_1999_2017 <- read.csv('./Midstone_Project/data/Heart_diseases_death_rate_gender_1999-2017.csv')
HD_deaths_Total_1999_2017 <- read.csv('./Midstone_Project/data/Heart_diseases_death_rate_Total_1999-2017.csv')
HD_deaths_Age_1999_2017 <- read.csv('./Midstone_Project/data/Heart_disease_death_rate_by_Age_1999-2017.csv')
HD_Race_Deaths_1999_2017 <- read.csv('./Midstone_Project/data/Heart_disease_death_rate_by_Race_1999-2017.csv')
HD_Total_Age_1999_2017 <- read.csv('./Midstone_Project/data/Age_Gender_Death_1999-2017.csv')

county <- read.csv('./Midstone_Project/data/county_FIPS.csv')
FFR <- read.csv('./Midstone_Project/data/FFR.csv')
county_cvd_2017 <- read.csv('./Midstone_Project/data/CVD_county_2017.csv',stringsAsFactors = FALSE, na = 'Unreliable')

# county_cvd_2015 <- read.csv('./Midstone_Project/data/County_cvd_death_2015.csv')
# county_cvd_2014 <- read.csv('./Midstone_Project/data/County_cvd_death_2014.csv')

#FFR_per_county <- read.csv('./Midstone_Project/data/FFR_per_county.csv')
county_FF_2017 <- read.csv('./Midstone_Project/data/county_FF_2017.csv')
#county_edu <- read.csv('./Midstone_Project/data/County_education.csv')

HD_Race_Deaths_by_Year<- read.csv('./Midstone_Project/data/Hear_disease_Race_Death_Rate_1999_2017.csv')

State_abbrev <- read.csv('./Midstone_Project/data/50_us_states_all_data.csv')
Age_deaths <- read.csv('./Midstone_Project/data/Age_deaths.csv')
County_geo_codes <- read.csv('./Midstone_Project/data/2017_counties_geo.csv')

 # county_cvd_2017$County %<>%
 #   gsub(" County", "",.) %>%
 #    gsub(",.*", "",.)
# # 
 #county_cvd_2017 <- county_cvd_2017 %>% select(-FIPS)
#county_cvd_2017 <- county_cvd_2017 %>% rename(State_name = 'State')
# cnty <- map_data("county")
# cnty <- cnty %>% mutate(region = capitalize(region),subregion = capitalize(subregion)) %>% 
#    rename(County = 'subregion', State_name = 'region') 
# FFR_county <- left_join(x=FFR, y=State_abbrev, by = 'State')
# cnty <- left_join(x= cnty, y= FFR_county, by= 'State_name')
# Full_dataframe_county <- left_join(x=county_cvd_2017,y=geo_codes,by='State_name')
# 
# #county_cvd_2017 <- county_cvd_2017 %>% select(Age.Adjusted.Rate,State,FIPS)
# 
# #Full_dataframe_county <- Full_dataframe_county %>% select(-State.y,-FIPS.y,-ANSICODE) %>% 
#  # rename(State='State.x', FIPS= 'FIPS.x')
# 
# Full_dataframe_county  <- left_join(x=Full_dataframe_county,y = county_cvd_2017, by = 'FIPS')
# colnames(Full_dataframe_county)
# 
# Full_dataframe_county <- Full_dataframe_county  %>% filter(!is.na(FIPS), !is.na(FFRPTH14)) %>% select(-State_name,-State.code)
# #Full_dataframe_county <- Full_dataframe_county %>% select(-State.y) %>% 
#   #rename(State='State.x')
# 
# #Full_dataframe_county <- Full_dataframe_county[!duplicated(Full_dataframe_county$order),]
#   
# cnty <- map_data("county")
# 
# state_df <- map_data("state")
# cnty <- cnty %>% mutate(region = capitalize(region),subregion = capitalize(subregion)) %>% 
#   rename(County = 'subregion', State_name = 'region') 
# Full_dataframe_county  <- full_join(x=Full_dataframe_county,y = cnty)
# 
# #Full_dataframe_county <- left_join(x=Full_dataframe_county ,y = cnty, by='County')
# 
# 
# write.csv(as.data.frame(Full_dataframe_county), file = './Midstone_Project/data/Full_dataframe_county')

# Top 15 causes of deaths in US. Probably from 1999-2017?. Age Adjusted rate. 
#data source: CDC wonder using under mortality 1999-2017

ggplot(Top_15_deaths, aes(x = reorder(disease, Age_Adjusted_Rate), y = Age_Adjusted_Rate)) +
 geom_bar(stat = 'identity', aes(fill = -Age_Adjusted_Rate), show.legend = FALSE) + coord_flip()+
  xlab('') +
  ylab('Age Adjusted Rate') +
  theme(text = element_text(size=15))+
  ggtitle('Top 15 causes of Death in US') +
  theme(axis.title.y = element_text(size = 4))

# Cardiovascular deaths by gender: 
HD_deaths_gender_1999_2017
HD_deaths_gender_x <-  HD_deaths_gender_1999_2017 %>% group_by(Gender) %>% summarise(Mean=mean(Age_Adjusted_Rate)) %>% arrange(desc(Mean))
HD_deaths_gender_x
  
ggplot(HD_deaths_gender_x, aes(x = reorder(Gender, Mean), y = Mean)) +
  geom_bar(stat = 'identity', aes(fill = -Mean), show.legend = FALSE) +
  xlab('') +
  ylab('Age Adjusted Death Rate (Mean) in the US') +
  theme(text = element_text(size=15))+
  ggtitle('Mortality Rate Male vs Female ') +
  theme(axis.title.x = element_text(size = 20))


# Plotting for age adjusted mortality rate from 1999-2017
HD_deaths_Total_1999_2017 <- HD_deaths_Total_1999_2017 %>% dplyr::select(State_name,State,Year,Age_Adjusted_Rate) %>% group_by(State_name, Year,Age_Adjusted_Rate) %>% arrange(desc(Age_Adjusted_Rate)) 
HD_deaths_Total_1999_2017
HD_deaths_Total_1999_2017_x <-HD_deaths_Total_1999_2017 %>% spread(Year,Age_Adjusted_Rate)%>% arrange(desc(`1999`))
HD_deaths_Total_1999_2017_x
State_names <- HD_deaths_Total_1999_2017_x$State_name
State_names[1:10]
Top_10_states <- HD_deaths_Total_1999_2017 %>% filter(State_name %in% State_names[1:10])
Top_10_states

ggplot(HD_deaths_Total_1999_2017 %>% filter(State_name == 'Mississippi'), aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
 # scale_x_continuous(labels = comma, limits = c(1999, 2017))+
  
  geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(150,400)+
  ggtitle('Age Adjusted Mortality from 1999 to 2017 ') +
  scale_x_continuous(breaks=seq(1999,2018,2))
  theme_bw()

# mortality rate by Gender
HD_deaths_gender <- HD_deaths_gender_1999_2017 %>% dplyr::select(State_name,State,Year,Gender, Age_Adjusted_Rate)
HD_deaths_gender_x <-HD_deaths_gender %>% spread(Year,Age_Adjusted_Rate)%>% arrange(desc(`1999`))
HD_deaths_gender_x
State_names <- HD_deaths_gender_x$State_name
State_names[1:5]
Top_10_states <- HD_deaths_gender %>% filter(State_name %in% State_names[1:5])
Top_10_states

g <- ggplot(Top_10_states %>% filter(Gender == 'Male'), aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
  # scale_x_continuous(labels = comma, limits = c(1999, 2017))+
  
  geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(0,450)+
  ggtitle('Age Adjusted Mortality from 1999 to 2017 ') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
theme_bw()

g1 <-  ggplot(Top_10_states %>% filter(Gender == 'Female'), aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
  # scale_x_continuous(labels = comma, limits = c(1999, 2017))+
  
geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(0,450)+
  ggtitle('Age Adjusted Mortality from 1999 to 2017 ') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
theme_bw()

ggarrange(g,g1,ncol = 2, common.legend = TRUE, legend="left")

# Prevalence based on gender 
prevalence_2013_2017 <- gather(HD_prevalence_2013_2017, 'Gender',"Percent","Male":"Female")
prevalence_2013_2017

prevalence_Gender <- prevalence_2013_2017 %>% group_by(Gender) %>% summarise(Mean=mean(Percent)) %>% arrange(desc(Mean))
prevalence_Gender

ggplot(prevalence_Gender, aes(x = reorder(Gender, Mean), y = Mean)) +
  geom_bar(stat = 'identity', aes(fill = -Mean), show.legend = FALSE) +
  xlab('') +
  ylab('Heart Disease \nPrevalence(Mean Percentage)') +
  theme(text = element_text(size=15))+
  ggtitle('Prevalence Gender Difference') +
  theme(axis.title.x = element_text(size = 20))



ggplot(prevalence_2013_2017 %>% filter(State_name == 'Tennessee'), aes(Year, Percent, group = Gender, color = Gender))+ 
  geom_line(size = 1) +
  #facet_wrap(~ Gender)+
  geom_point(size=3, shape=21, aes(fill=factor(Gender)))+
  labs(color = "Gender", fill = 'Gender')+
  ylab('Crude Death Rate)') +
  xlab('Year') +
  ylim(0, 15)+
  ggtitle('Crude Death Rate by Age Group from 1999 to 2017 Male ') +
  scale_x_continuous(breaks=seq(2013,2018,1))+
  theme(text = element_text(size=14))

# plotting of prevalance based year wise

prevalence_gender_map <- prevalence_2013_2017 %>% group_by(State,Gender,Year) %>% summarise(sum=sum(Percent))
prevalence_2013_2017
prevalence_gender_map 
  l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)

  plot_geo(prevalence_2013_2017 %>% filter(Year == 2016, Gender == 'Female'), locationmode = 'USA-states') %>%
  add_trace(
    z = ~Percent, locations = ~State,
    colors = 'Blues'
  ) %>%
  colorbar(title = "Percentage \nHeart Disease") %>%
  layout(
    title = 'Percentage Heart Disease Prevalence',
    titlefont = list(size = 24),
    margin = list(t = 50), 
    geo = g,
    annotations = 
      list(x = 1, y = -0.1, text = "Data Source: Henry J Kaiser Family Foundation", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=12)))

# plotting of Total Death Rate 
  
 y <- HD_deaths_Total_1999_2017 %>% group_by(State,Year) %>% summarise(sum=sum(Age_Adjusted_Rate))
 y 
 

  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE,
    lakecolor = toRGB('white')
  )
  
  plot_geo(y %>% filter(Year == 2016), locationmode = 'USA-states') %>%
    add_trace(
      z = ~sum, locations = ~State,zmin = 115, zmax = 348,
      colors = 'Reds'
    ) %>%
    colorbar(title = "Age Adjusted \nDeath Rate") %>%
    layout(
      title = 'Age Adjusted Death Rate Heart Diseases',
      titlefont = list(size = 24),
      margin = list(t = 50), 
      geo = g,
      annotations = 
        list(x = 1, y = -0.1, text = "Data Source: Henry J Kaiser Family Foundation", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=12)))
# looking at race deaths 
summary(HD_Race_Deaths_1999_2017)

mean_race_deaths <- HD_Race_Deaths_1999_2017 %>% group_by(Race) %>% summarise(Mean=mean(Age_Adjusted_Rate))
ggplot(mean_race_deaths, aes(x = reorder(Race, Mean), y = Mean)) +
  geom_bar(stat = 'identity', aes(fill = -Mean), show.legend = FALSE) +
  xlab('') +
  ylab('Age Adjusted Death Rate(Mean) in the US') +
  theme(text = element_text(size=15))+
  ggtitle('Mortality Rate') +
  theme(axis.title.x = element_text(size = 20))

head(HD_Race_Deaths_1999_2017)

#Mapping Race 
l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)

plot_geo(HD_Race_Deaths_1999_2017 %>% filter(Race == 'Asian or Pacific Islander'), locationmode = 'USA-states') %>%
  add_trace(
    z = ~Age_Adjusted_Rate, locations = ~State,zmin = 60, zmax = 160,
    colors = 'Reds'
  ) %>%
  colorbar(title = "Age Adjusted \nDeath Rate") %>%
  layout(
    title = 'Age Adjusted Death Rate Heart Diseases',
    titlefont = list(size = 24),
    margin = list(t = 50), 
    geo = g,
    annotations = 
      list(x = 1, y = -0.1, text = "Data Source: Henry J Kaiser Family Foundation", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=12)))


#Total Age in US

a <- ggplot(HD_Total_Age_1999_2017 %>% filter(Gender == 'Male'), aes(x = reorder(Age_group, Crude.Rate), y = Crude.Rate)) +
  geom_bar(stat = 'identity', aes(fill = -Crude.Rate), show.legend = FALSE) + coord_flip()+
  xlab('') +
  ylab('Age Adjusted Rate') +
  theme(text = element_text(size=15))+
   ggtitle('Death rate with Age groups with Heart Disease ') +
  theme(axis.title.y = element_text(size = 4))


b <- ggplot(HD_Total_Age_1999_2017 %>% filter(Gender == 'Female'), aes(x = reorder(Age_group, Crude.Rate), y = Crude.Rate)) +
    geom_bar(stat = 'identity', aes(fill = -Crude.Rate), show.legend = FALSE) + coord_flip()+
    xlab('') +
    ylab('Age Adjusted Rate') +
    theme(text = element_text(size=15))+
    scale_y_continuous( trans = 'reverse')+
  ggtitle('Death rate with Age groups with Heart Disease ') +
    theme(axis.title.y = element_text(size = 4))

ggarrange(b,a,ncol = 2, common.legend = TRUE, legend="left")

# Bar plot side by side 
ggplot(data = HD_Total_Age_1999_2017, 
             aes(reorder(Age_group, Crude.Rate), Crude.Rate)) +
  geom_bar(stat = 'identity',position="dodge", aes(fill = Gender)) + coord_flip()+
  xlab('Age Group') +
  ylab('Crude Death Rate') +
    ggtitle('Age dependent Death rate by Gender')+
  theme_bw()
  
  # Gender Ratio
Gender_Ratio_State <- read.csv('./Midstone_Project/data/Gender_Age_group_crude_by_state.csv')


spread <- Gender_Ratio_State %>%  
  spread(Gender, Crude_Rate)

write.csv(as.data.frame(spread), file = './Midstone_Project/data/genderspread.csv')


# %>% 
#   group_by(State,Age_group) %>% 
#   mutate(ratio = Male/Female)




x <- HD_Total_Age_1999_2017 %>% filter(!is.na(Ratio)))
x


 ggplotly(ggplot(data =x,aes(group, Ratio)) +
    geom_bar(stat = 'identity', aes(fill = -Ratio), show.legend = FALSE)+
    xlab('Age Group') +
    ylab('Relative Death Ratio Male to Female') +
    
    ggtitle('Relative Death Ratio by Gender')+
  theme_bw())

 HD_Total_Age_1999_2017


# Age deaths by year and State

ggplot(Age_deaths %>% filter(Year == 2017,State =='TN'), aes(reorder(Age_group, Crude_Rate),Crude_Rate)) +
  geom_bar(stat = 'identity',aes(fill = Age_group), show.legend = FALSE) + coord_flip()+
  xlab('') +
  ylab('Age Adjusted Rate') +
  theme(text = element_text(size=15))+
  ggtitle('Death rate with Age groups with Heart Disease TN') +
  theme(axis.title.y = element_text(size = 4))
# 
# HD_deaths_Age_1999_2017$Crude_Rate<-  gsub("Unreliable", "NA",HD_deaths_Age_1999_2017$Crude_Rate)
# age_deaths <-  HD_deaths_Age_1999_2017  %>% mutate(Crude_Rate=as.numeric(Crude_Rate)) %>% filter(!is.na(Crude_Rate)) 
# str(age_deaths)
#write.csv(as.data.frame(age_deaths), file = './Midstone_Project/data/age_deaths.csv')

#a <-  HD_deaths_Age_1999_2017 %>% filter(Year == 1999|State == 'Alabama') %>% group_by(State, Age_group) %>% summarise(sum=sum(Crude_rate_per_100000))

HD_Race_Deaths_by_Year

z <-   ggplot(HD_Race_Deaths_by_Year, aes(Year, Age_Adjusted_Rate, group = Race, color = Race)) + 
  geom_line(size = 1) +
  geom_point(size=3, shape=21, aes(fill=factor(Race)))+
  labs(color = "Race", fill = 'Race')+
  ylab('Age Adjusted Death Rate') +
  xlab('Year') +
  ylim(0, 500)+
  ggtitle('Age Adjusted Death Rate by Race') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
  theme_bw()

z

Gender_based_year <- read.csv('./Midstone_Project/data/Gender_Age_deaths_only_years_1999_2017.csv')

g <- ggplot(Gender_based_year %>% filter(Gender == 'Male'), aes(Year, Crude_Rate, group = Age_group, color = Age_group)) + 
  geom_line(size = 1) +
  #facet_wrap(~ Gender)+
  # scale_x_continuous(labels = comma, limits = c(1999, 2017))+
  
  geom_point(size=3, shape=21, aes(fill=factor(Age_group)))+
  labs(color = "Age_group", fill = 'Age_group')+
  ylab('Crude Death Rate)') +
  xlab('Year') +
  ylim(0, 7000)+
  ggtitle('Crude Death Rate by Age Group from 1999 to 2017 Male ') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
  theme_bw()

g

g1 <- ggplot(Gender_based_year %>% filter(Gender == 'Female'), aes(Year, Crude_Rate, group = Age_group, color = Age_group)) + 
  geom_line(size = 1) +
  #facet_wrap(~ Gender)+
  # scale_x_continuous(labels = comma, limits = c(1999, 2017))+
  
  geom_point(size=3, shape=21, aes(fill=factor(Age_group)))+
  labs(color = "Age_group", fill = 'Age_group')+
  ylab('Crude Death Rate)') +
  xlab('Year') +
  ylim(0, 7000)+
  ggtitle('Crude Death Rate by Age Group from 1999 to 2017  Female ') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
  theme_bw()
g1 
ggarrange(g,g1,ncol = 2, common.legend = TRUE, legend="left",label.y= 2)
?ggarrange




## county

#********** dont delete**************
# cnty <- map_data("county")
# state_df <- map_data("state")
# #
# cnty <- cnty %>% mutate(region = capitalize(region),subregion = capitalize(subregion)) %>% rename(County = 'subregion', State_name = 'region') %>% select(-lat,-long)
# 
# #FFR <- FFR %>% mutate(County = as.character(County))
# FFR_per_county <- inner_join(cnty, FFR, by = "County")
# FFR_per_county <- FFR_per_county[!duplicated(FFR_per_county$order),]
# FFR_per_county <- inner_join(FFR, FFR_per_county, by = "County")
# FFR_per_county <- inner_join(county_cvd_2017, FFR_per_county, by = "County")
# 
# FFR_per_county <- FFR_per_county[!duplicated(FFR_per_county$FIPS),]
# #FFR_per_county <- FFR_per_county %>% select(-FIPS.x) %>% rename(FIPS = 'FIPS.y')
# #FFR_per_county <- FFR_per_county[!duplicated(FFR_per_county$FFRPTH14),]
# 
# FFR_per_county<-FFR_per_county %>% filter(FFRPTH14 >0)
#write.csv(as.data.frame(FFR_per_county), file = './Midstone_Project/data/FFR_per_county.csv')
#county_cvd_2017 <- mutate_all(county_cvd_2017 , funs(replace(., .=='Unreliable', NA)))
#FFR_per_county <- FFR_per_county %>% mutate(Age.Adjusted.Rate= as.numeric(Age.Adjusted.Rate)) %>% filter(!is.na(Age.Adjusted.Rate))
#FFR_per_county <- FFR_per_county[!duplicated(FFR_per_county$order),]
#county_cvd_2017 <- county_cvd_2017 %>% filter(!is.na(Age.Adjusted.Rate))
#write.csv(as.data.frame(county_cvd_2017), file = './Midstone_Project/data/county_cvd_2017.csv')
# 
# p <- ggplot(county, aes(long, lat,fill=FFRPTH14,
#                                      text=paste('</br>County :',County,'</br>State :',State_name))) + 
#   geom_polygon(colour = alpha("black", 1/6))+
#   labs(fill = "FFR per 1000 \ncounty residents")  +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
#   scale_fill_continuous(low="thistle2", high="darkred", 
#                         guide="colorbar",na.value="white")+
#   theme(rect = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank())
# 
# p <- ggplotly(p)
# p
# 
# p1 <- ggplot(data=FFR_per_county, aes(long, lat,group = group, fill = FSRPTH14,
#                                       text=paste('</br>County :',County,'</br>State :',State))) + 
#   geom_polygon(colour = alpha("white", 1/6))+
#   labs(fill = "FSR per 1000 \ncounty residents")  +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
#   scale_fill_continuous(low="thistle2", high="darkred", 
#                         guide="colorbar",na.value="white")+
#   theme(rect = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank())
# p1 <- ggplotly(p1)
# p1

#write.csv(as.data.frame(ab), file = './Midstone_Project/data/ab.csv')

#write.csv(as.data.frame(ab), file = './Midstone_Project/data/Fast_Food_final.csv')

#ab<- read.csv('./Midstone_Project/data/ab.csv')

# Fast_Food_final <- Fast_Food_final %>% select(-State, -X)

# Fast_Food_final$FFRPTH14 <- Fast_Food_final$FFRPTH14*100


#ab$FFRPTH14 <- ab$FFRPTH14*1000

# household_data %>%
#   ggplot(aes(long, lat, group = group, fill = medhhincome)) +
#   geom_polygon(color = NA) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   labs(fill = "Median Household Income")



# 
# p <- ggplotly(p, tooltip = 'text') %>% 
#   layout(
#     hovermode = 'x',
#     margin = list(
#       t = 20,
#       b = 20,
#       l = 20,
#       r = 20),
#     legend = list(
#       orientation = 'h',
#       x = 0.5,
#       y = 1.01,
#       xanchor = 'center'))
# 
# # use style to modify layer
# #p <- style(p, hoverinfo = 'none', traces = c(3))
# 
# # use plotly_build to modify layer
# p <- plotly_build(p)
# str(p$x$layout$annotations) # check annotations
# p$x$layout$annotations = NULL # remove annotation
# p
# 
# #county <- county %>% select(-State_name,-group,-X)
# #county
# # county <- merge(x=county,y = FFR, by="FIPS", all.y = TRUE)
# # county
# # write.csv(as.data.frame(county), file = './Midstone_Project/data/county_FIPS.csv')
# #cnty <- merge(x=cnty,y = State_abbrev, by.y="State_name", all.y = TRUE)
# #county <- read.csv('./Midstone_Project/data/FFR.csv')
# #county <- read.csv('./Midstone_Project/data/county.csv')
# # FFR_x <- merge(x=FFR,y = county, by="County", all.x = TRUE, all.y = TRUE)
# # write.csv(as.data.frame(FFR_x), file = './Midstone_Project/data/FFR_x.csv')
# 
# 
# 
# #FFR <- FFR[!duplicated(county$FFRPTH14),]
# Fast_Food <- FFR %>% group_by(State) %>%  summarize(sum=sum(FFRPTH14))
# #Fast_Food <- Fast_Food  %>% filter(!is.na(FFRPTH14)) 
# # Fast_Food
# # 
# # x <- Fast_Food %>% select(State, FIPS, lat, long,FFRPTH14)
# # head(x)
# 
# l <- list(color = toRGB("white"), width = 2)
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = FALSE,
#   lakecolor = toRGB('white')
# )
# 
# plot_geo(Fast_Food, locationmode = 'USA-states') %>%
#   add_trace(
#     z = ~sum, locations = ~State,
#     colors = 'Reds'
#   ) %>%
#   colorbar(title = "Sum of FFR") %>%
#   layout(
#     title = 'FFR USA',
#     titlefont = list(size = 24),
#     margin = list(t = 50),
#     geo = g,
#     annotations =
#       list(x = 1, y = -0.1, text = "Data Source: Datainfinity",
#            showarrow = F, xref='paper', yref='paper',
#            xanchor='right', yanchor='auto', xshift=0, yshift=0,
#            font=list(size=12)))
# 
# # county deaths
# county_death <- read.csv('./Midstone_Project/data/county_Death_Rate_State_1999_2017.csv')
# county_state_geo <- read.csv('./Midstone_Project/data/Counties_state_geo.csv')
# 
# county_death$County %<>% 
#     gsub(" County", "",.) %>%
#     gsub(",.*", "",.)
# 
# county_state_geo$County %<>% 
#   gsub(" County", "",.)
#   
# county_death_rate <- inner_join(county_death, cnty, by = "County")
# 
# 
# str(FFR_per_county)
# TN <- FFR_per_county %>% filter(State =='TN') %>%  mutate(Age_Adjusted_Rate = as.numeric(Age_Adjusted_Rate))
# #TN <- TN[!duplicated(TN$order),]
# p <- ggplot(data=TN, aes(long, lat,group = group, fill = Age_Adjusted_Rate,
#                                 text=paste('</br>County :',County))) + 
#   geom_polygon(colour = NA)+
#   labs(fill = "FSR per 1000 \ncounty residents")  +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
#   scale_fill_continuous(low="thistle2", high="darkred", 
#                         guide="colorbar",na.value="white")+
#   theme(rect = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank())
# 
# p <- ggplotly(p)
# p
# 
# ggplot(TN, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(aes(fill = group), color = NA)


#********* cleaning county 2017 data for correlation

# county_cvd_2017$County %<>%
#    gsub(" County", "",.) %>%
#    gsub(",.*", "",.)
#  county_FF_2017 <- inner_join(county_cvd_2017, FFR, by = "County")
# #  
#  
#  county_FF_2017$Age.Adjusted.Rate<-  gsub("Unreliable", "NA",county_FF_2017$Age.Adjusted.Rate)
#  county_FF_2017 <- county_FF_2017 %>% rename(State_name ='State.x', State= 'State.y') %>%
#    select(State_name,State,County, FIPS,group,order, long,lat,Crude.Rate,Age.Adjusted.Rate, FFRPTH14,FSRPTH14,
#           RECFACPTH14,POVRATE15,PCT_NHWHITE10,PCT_NHBLACK10,PCT_HISP10,PCT_NHASIAN10,PCT_NHASIAN10,PCT_NHNA10,PCT_NHPI10,
#           Population.Estimate..2014,Population.Estimate..2015,Population.Estimate..2016) %>%
#    mutate(Age.Adjusted.Rate = as.numeric(Age.Adjusted.Rate)) %>% filter(!is.na(Age.Adjusted.Rate))
# 
#  any(is.na(county_FF_2017$Age.Adjusted.Rate))
#  colnames(county_FF_2017)
#  str(county_FF_2015)
#write.csv(as.data.frame(county_FF_2017), file = './Midstone_Project/data/county_FF_2017.csv')

# county_FF_2017 <- county_FF_2017[!duplicated(county_FF_2017$order),]
#write.csv(as.data.frame(county_edu), file = './Midstone_Project/data/county_FF_2017.csv')
# county_edu <- county_edu %>% rename(County = 'Area.name', PCTHIGSD= 'Percent.of.adults.with.a.bachelor.s.degree.or.higher..2012.2016')
# colnames(county_edu)
# county_edu$County %<>%
#       gsub(" County", "",.) %>%
#       gsub(",.*", "",.)
# 
# # county_edu <- inner_join(county_edu, county_FF_2017, by = "County")
# corr_age <- county_FF_2017 %>% select(State_name,County,Age.Adjusted.Rate,RECFACPTH14,FFRPTH14,FSRPTH14,POVRATE15,RECFACPTH14) #%>% mutate(Age.Adjusted.Rate =as.numeric(Age.Adjusted.Rate))
# #corrplot <- corrplot %>% group_by(State_name,County) %>% summarise(sum= sum(FFRPTH14))
# corr_age <- corr_age[!duplicated(corr_age$FFRPTH14),]
# 
# colnames(corr_age)
# 
# 
# corr_age[, 5:6] <- log2(corr_age[,5:6]+1)
# num.cols <- sapply(corr_age,is.numeric)
# cor.data <- cor(corr_age[,num.cols])
# print(cor.data)
# print(corrplot(cor.data, method = 'circle'))
# 
# county_cvd_2014$County %<>% 
#   gsub(" County", "",.) %>%
#   gsub(",.*", "",.)
# county_FF_2014 <- inner_join(county_cvd_2014, FFR_per_county, by = "County")
# 
# county_FF_2014 <- county_FF_2014 %>% rename(State_name ='State.x', State= 'State.y') %>% 
#       select(State_name,State,County, FIPS,group,order, long,lat,Age.Adjusted.Rate, FFRPTH14,FSRPTH14) 
#     
# any(is.na(county_FF_2014))
# str(county_FF_2014)

#ccccc <- county_cvd_2017 %>% filter(!is.na(Age.Adjusted.Rate))


## Saving RDS files 
saveRDS(HD_deaths_Total_1999_2017, file = 'HD_Total_deaths_1999_2017.RDS')
saveRDS(Top_15_deaths, file = 'Top_15_deaths.RDS')
saveRDS(HD_Total_Age_1999_2017, file = 'Gender_Ratios_from_1999_2017.RDS')
saveRDS(prevalence_2013_2017, file = 'Gender_Difference_Prevalance_2013_2017.RDS')
saveRDS(prevalence_2013_2017, file = 'Prevalence_heart_disease_2013_2017.RDS')
saveRDS(FFR, file = 'FFR.RDS')
saveRDS(county_cvd_2017, file = 'county_cvd_2017.RDS')
saveRDS(HD_Race_Deaths_by_Year, file = 'HD_Deaths_by_race_by_Year.RDS')
saveRDS(Gender_based_year, file = 'Gender_based_CrudeDeath_1999_2017.RDS')
saveRDS(HD_Race_Deaths_1999_2017, file = 'HD_Race_Deaths_map_1999_2017.RDS')
Top_15_deaths <- readRDS(file ='Top_15_deaths.RDS')
