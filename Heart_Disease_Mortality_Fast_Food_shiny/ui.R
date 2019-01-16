


shinyUI(fluidPage(
  theme = shinytheme("darkly"), 
    fluidRow(
    column(8, offset = 3, titlePanel("Heart Disease Mortality and Fast Food Restaurants Density in US"))),
  
  sidebarLayout(
    sidebarPanel(
      tags$blockquote("About 610,000 people die of heart disease 
                                               in the United States every year–that's 1 in every 4 deaths"),    
      tags$br("Data Source:"),
      tags$p("CDC Wonder"),
      tags$p("USDA-Food Environment Atlas"),
      tags$br(""),
      tags$br(""),
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 14px; }"))),width = 2,
       
                 selectInput("state", 
                            label = "Select a State", 
                            choices = states,
                            selected = 'State_name',selectize = FALSE),
                  selectInput("var", 
                             label = "Select a variable", 
                             choices = c("Age Adjusted Rate" = 'Age_Adjusted_Rate',
                                         "Percent Adult diabetes 2013" = "Percent_Adults_Diabetes_2013",
                                         "Percent Adult Obese  2013" = "Percent_Adults_Obese_2013" ,
                                         "Recreational facilties % 09 14"="Rec_Facilities_per_1000_09_14",
                                         "% Population 65 years or older 2010"="PCT_65OLDER10"),
                             selectize = FALSE),
      tags$br("Note:"),          
      tags$p("Click on County: Selected Variable info"),
      tags$br("Click on circle: Info about FFR and Heart Disease Mortality Rate")),
  
mainPanel(
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 20px; }"))),
      fluidRow(
        leafletOutput("FFR_tmap",width = "125%")),
      fluidRow(
      leafletOutput("FFR_TN", width = "125%"))
    )
)
)
)

