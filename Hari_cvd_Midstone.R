setwd("/Users/hari/NSSdatascience/R/Heart_Disease_Mortality_Fast_Food")

library(tidyr)
library(tidyverse)
library(ggpubr)
library(plotly)
library(R.utils)
library(janitor)
library(ggcorrplot)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)


#HD : cardiovascular disease

HD_prevalence_2013_2017 <- read.csv('./data/Cardiovascular_prevalence_2013-2017_kff.csv')
Top_15_deaths <- read.csv('./data/Top_15_causes_death_1999-2017.csv')
HD_deaths_gender_1999_2017 <- read.csv('./data/Heart_diseases_death_rate_gender_1999-2017.csv')
HD_deaths_Total_1999_2017 <- read.csv('./data/Heart_diseases_death_rate_Total_1999-2017.csv')
HD_deaths_Age_1999_2017 <- read.csv('./data/Heart_disease_death_rate_by_Age_1999-2017.csv')
HD_Race_Deaths_1999_2017 <- read.csv('./data/Heart_disease_death_rate_by_Race_1999-2017.csv')
HD_Total_Age_1999_2017 <- read.csv('./data/Age_Gender_Death_1999-2017.csv')
FFR <- read.csv('./data/FFR.csv')
county_cvd_2017 <- read.csv('./data/CVD_county_2017.csv',stringsAsFactors = FALSE, na = 'Unreliable')
HD_Race_Deaths_by_Year<- read.csv('./data/Hear_disease_Race_Death_Rate_1999_2017.csv')
State_abbrev <- read.csv('./data/50_us_states_all_data.csv')
Age_deaths <- read.csv('./data/Age_deaths.csv')
Gender_based_year <- read.csv('./data/Gender_Age_deaths_only_years_1999_2017.csv')

# Top Causes of Death in US including Heart diseases
ggplot(Top_15_deaths, aes(x = reorder(disease, Age_Adjusted_Rate), y = Age_Adjusted_Rate)) +
 geom_bar(stat = 'identity', aes(fill = -Age_Adjusted_Rate), show.legend = FALSE) + coord_flip()+
  xlab('') +
  ylab('Age Adjusted Rate') +
  theme(text = element_text(size=15))+
  ggtitle('Top 15 causes of Death in US') +
  theme(axis.title.y = element_text(size = 4))

# Cardiovascular deaths by gender: Plot mean value of Gender
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


# Plotting for age adjusted mortality rate from 1999-2017. Plotting for top 10 states
HD_deaths_Total_1999_2017 <- HD_deaths_Total_1999_2017 %>% dplyr::select(State_name,State,Year,Age_Adjusted_Rate) %>% group_by(State_name, Year,Age_Adjusted_Rate) %>% arrange(desc(Age_Adjusted_Rate)) 
HD_deaths_Total_1999_2017
HD_deaths_Total_1999_2017_spread <-HD_deaths_Total_1999_2017 %>% spread(Year,Age_Adjusted_Rate)%>% arrange(desc(`1999`))
HD_deaths_Total_1999_2017_spread
State_names <- HD_deaths_Total_1999_2017_spread$State_name
State_names[1:10]
Top_10_states <- HD_deaths_Total_1999_2017 %>% filter(State_name %in% State_names[1:10])
Top_10_states

ggplot(Top_10_states, aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
  geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(150,400)+
  ggtitle('Age Adjusted Mortality from 1999 to 2017 ') +
  scale_x_continuous(breaks=seq(1999,2018,2))
  theme_bw()

# mortality rate by Gender from 1999 to 2017
HD_deaths_gender <- HD_deaths_gender_1999_2017 %>% dplyr::select(State_name,State,Year,Gender, Age_Adjusted_Rate)
HD_deaths_gender_x <-HD_deaths_gender %>% spread(Year,Age_Adjusted_Rate)%>% arrange(desc(`1999`))
HD_deaths_gender_x
State_names <- HD_deaths_gender_x$State_name
State_names[1:5]
Top_10_states <- HD_deaths_gender %>% filter(State_name %in% State_names[1:5])
Top_10_states

g <- ggplot(Top_10_states %>% filter(Gender == 'Male'), aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
  geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(0,450)+
  ggtitle('Age Adjusted Mortality Rate Male ') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
theme_bw()

g1 <-  ggplot(Top_10_states %>% filter(Gender == 'Female'), aes(Year, Age_Adjusted_Rate, group = State_name, color = State_name)) + 
  geom_line(size = 1) +
geom_point(size=3, shape=21, aes(fill=factor(State_name)))+
  labs(color = "US State", fill = 'US State')+
  ylab('Death Rate(Age Adjusted)') +
  xlab('Year') +
  ylim(0,450)+
  ggtitle('Age Adjusted Mortality Female') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
theme_bw()

ggarrange(g,g1,ncol = 2, common.legend = TRUE, legend="left")

# Prevalence based on gender. Plot mean value of gender for prevalence. Data from Kaiser family Foundation
prevalence_2013_2017 <- gather(HD_prevalence_2013_2017, 'Gender',"Percent","Male":"Female")
prevalence_Gender <- prevalence_2013_2017 %>% group_by(Gender) %>% summarise(Mean=mean(Percent)) %>% arrange(desc(Mean))
ggplot(prevalence_Gender, aes(x = reorder(Gender, Mean), y = Mean)) +
  geom_bar(stat = 'identity', aes(fill = -Mean), show.legend = FALSE) +
  xlab('') +
  ylab('Heart Disease \nPrevalence(Mean Percentage)') +
  theme(text = element_text(size=15))+
  ggtitle('Prevalence Gender Difference') +
  theme(axis.title.x = element_text(size = 20))


# Heart disease prevalence by gender in State of Tennessee from 2013-2017
ggplot(prevalence_2013_2017 %>% filter(State_name == 'Tennessee'), aes(Year, Percent, group = Gender, color = Gender))+ 
  geom_line(size = 1) +
  geom_point(size=3, shape=21, aes(fill=factor(Gender)))+
  labs(color = "Gender", fill = 'Gender')+
  ylab('Crude Death Rate') +
  xlab('Year') +
  ylim(0, 15)+
  ggtitle('Crude Death Rate by Age Group from 1999 to 2017 Male ') +
  scale_x_continuous(breaks=seq(2013,2018,1))+
  theme(text = element_text(size=14))

# Plotly map to plot Heart Disease prevalence. Changing filter to to different year and gender.

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
  plot_geo(prevalence_2013_2017 %>% filter(Year == 2017, Gender == 'Female'), locationmode = 'USA-states') %>%
  add_trace(
    z = ~Percent, locations = ~State,
    colors = 'Blues'
  ) %>%
  colorbar(title = "Percentage \nHeart Disease") %>%
  layout(
    title = 'Percentage Heart Disease Prevalence Year 2017 and Female',
    titlefont = list(size = 12),
    margin = list(t = 50), 
    geo = g,
    annotations = 
      list(x = 1, y = -0.1, text = "Data Source: Henry J Kaiser Family Foundation", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=12)))

# plotting of Heart disease deathrate from 1999 to 2017. Deathrate based on CDC wonder grouping year and age_adjusted rate
  
  HD_deaths_Total_1999_2017 <- HD_deaths_Total_1999_2017 %>% group_by(State,Year) %>% summarise(sum=sum(Age_Adjusted_Rate))
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE,
    lakecolor = toRGB('white')
  )
  
  plot_geo(HD_deaths_Total_1999_2017 %>% filter(Year == 2017), locationmode = 'USA-states') %>%
    add_trace(
      z = ~sum, locations = ~State,zmin = 115, zmax = 348,
      colors = 'Reds'
    ) %>%
    colorbar(title = "Age Adjusted \nDeath Rate") %>%
    layout(
      title = 'Age Adjusted Death Rate Heart Diseases 2017',
      titlefont = list(size = 24),
      margin = list(t = 50), 
      geo = g,
      annotations = 
        list(x = 1, y = -0.1, text = "Data Source: CDC Wonder", 
             showarrow = F, xref='paper', yref='paper', 
             xanchor='right', yanchor='auto', xshift=0, yshift=0,
             font=list(size=12)))
# Mean Death rate by Rate from 1999 to 2017
  
mean_race_deaths <- HD_Race_Deaths_1999_2017 %>% group_by(Race) %>% summarise(Mean=mean(Age_Adjusted_Rate))
ggplot(mean_race_deaths, aes(x = reorder(Race, Mean), y = Mean)) +
  geom_bar(stat = 'identity', aes(fill = -Mean), show.legend = FALSE) +
  xlab('') +
  ylab('Age Adjusted Death Rate(Mean) in the US') +
  theme(text = element_text(size=15))+
  ggtitle('Mortality Rate') +
  theme(axis.title.x = element_text(size = 20))


#Mapping death rate by race. 

l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)

plot_geo(HD_Race_Deaths_1999_2017 %>% filter(Race == 'Black or African American'), locationmode = 'USA-states') %>%
  add_trace(
    z = ~Age_Adjusted_Rate, locations = ~State,
    colors = 'Reds'
  ) %>%
  colorbar(title = "Age Adjusted \nDeath Rate") %>%
  layout(
    title = 'Age Adjusted Death Rate Heart Diseases',
    titlefont = list(size = 24),
    margin = list(t = 50), 
    geo = g,
    annotations = 
      list(x = 1, y = -0.1, text = "Data Source: CDC Wonder", 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=12)))

#Gender Plot by Age group
ggplot(data = HD_Total_Age_1999_2017, 
             aes(reorder(Age_group, Crude.Rate), Crude.Rate)) +
  geom_bar(stat = 'identity',position="dodge", aes(fill = Gender)) + coord_flip()+
  xlab('Age Group') +
  ylab('Crude Death Rate') +
    ggtitle('Age dependent Death rate by Gender')+
  theme_bw()
  
# Gender Ratio exploring
# Gender_Ratio_State <- read.csv('./data/Gender_Age_group_crude_by_state.csv')
# spread <- Gender_Ratio_State %>%  
#   spread(Gender, Crude_Rate)
# write.csv(as.data.frame(spread), file = './data/genderspread.csv')
# Gender_Ratio_State

genderratio <- HD_Total_Age_1999_2017 %>% filter(!is.na(Ratio))

 ggplotly(ggplot(data =genderratio,aes(group, Ratio)) +
    geom_bar(stat = 'identity', aes(fill = -Ratio), show.legend = FALSE)+
    xlab('Age Group') +
    ylab('Relative Death Ratio Male to Female') +
    ggtitle('Relative Death Ratio by Gender')+
  theme_bw())


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

# Race Trend in Heart disease mortality rate from 1999 to 2017.

ggplot(HD_Race_Deaths_by_Year, aes(Year, Age_Adjusted_Rate, group = Race, color = Race)) + 
  geom_line(size = 1) +
  geom_point(size=3, shape=21, aes(fill=factor(Race)))+
  labs(color = "Race", fill = 'Race')+
  ylab('Age Adjusted Death Rate') +
  xlab('Year') +
  ylim(0, 500)+
  ggtitle('Age Adjusted Death Rate by Race') +
  scale_x_continuous(breaks=seq(1999,2018,2))+
  theme_bw()


# Trend in Heart disease rate by gender groupe by age group


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


#********* cleaning county 2017 data for correlation

 county_FF_2017 <- inner_join(county_cvd_2017, FFR, by = "County")

names(county_FF_2017)
 county_FF_2017$Age.Adjusted.Rate<-  gsub("Unreliable", "NA",county_FF_2017$Age.Adjusted.Rate)
 county_FF_2017 <- county_FF_2017 %>% rename(State_name ='State.x', State= 'State.y') %>%
   
   mutate(Age.Adjusted.Rate = as.numeric(Age.Adjusted.Rate)) %>% filter(!is.na(Age.Adjusted.Rate))



 county_FF_2017 <- county_FF_2017 %>% drop_na()
 sum(is.na(county_FF_2017))
county_FF_2017 <- county_FF_2017[!duplicated(county_FF_2017$FFRPTH14),]

# county_edu <- county_edu %>% rename(County = 'Area.name', PCTHIGSD= 'Percent.of.adults.with.a.bachelor.s.degree.or.higher..2012.2016')
# colnames(county_edu)
# county_edu$County %<>%
#       gsub(" County", "",.) %>%
#       gsub(",.*", "",.)

# county_edu <- inner_join(county_edu, county_FF_2017, by = "County")
corr_age <- county_FF_2017 %>% select(Age.Adjusted.Rate,FFR14, Percent_Change_FFR_2009_2014,FFRPTH14,
                                   Poverty_rate_15,PCH_RECFACPTH_09_14,PCT_NHBLACK10,PCT_65OLDER10) #%>% mutate(Age.Adjusted.Rate =as.numeric(Age.Adjusted.Rate))


str(corr_age)
sum(is.na(corr_age))
#corr_age[, 1:8] <- log2(corr_age[,1:8])
num.cols <- sapply(corr_age,is.numeric)
cor.data <- cor(corr_age[,num.cols])
print(cor.data)
print(corrplot(cor.data, method = 'circle'))

ggplot(corr_age, aes(y=Age.Adjusted.Rate, x=FFRPTH14)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

ggplot(corr_age, aes(y=Age.Adjusted.Rate, x=Percent_Change_FFR_2009_2014)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

ggplot(corr_age, aes(y=Age.Adjusted.Rate, x=PCT_NHBLACK10)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

ggplot(corr_age, aes(y=Age.Adjusted.Rate, x=PCT_65OLDER10)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
## Saving RDS files 
# saveRDS(HD_deaths_Total_1999_2017, file = 'HD_Total_deaths_1999_2017.RDS')
# saveRDS(Top_15_deaths, file = 'Top_15_deaths.RDS')
# saveRDS(HD_Total_Age_1999_2017, file = 'Gender_Ratios_from_1999_2017.RDS')
# saveRDS(prevalence_2013_2017, file = 'Gender_Difference_Prevalance_2013_2017.RDS')
# saveRDS(prevalence_2013_2017, file = 'Prevalence_heart_disease_2013_2017.RDS')
# saveRDS(FFR, file = 'FFR.RDS')
# saveRDS(county_cvd_2017, file = 'county_cvd_2017.RDS')
# saveRDS(HD_Race_Deaths_by_Year, file = 'HD_Deaths_by_race_by_Year.RDS')
# saveRDS(Gender_based_year, file = 'Gender_based_CrudeDeath_1999_2017.RDS')
# saveRDS(HD_Race_Deaths_1999_2017, file = 'HD_Race_Deaths_map_1999_2017.RDS')
# Top_15_deaths <- readRDS(file ='Top_15_deaths.RDS')
