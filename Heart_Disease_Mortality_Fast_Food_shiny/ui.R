
shinyUI(fluidPage(
  theme = shinytheme("darkly"), 
    fluidRow(
      column(8, offset = 3, titlePanel("Heart Disease Mortality and Fast Food Restaurants Density in US"))),
  
    sidebarLayout(
      sidebarPanel(width = 3,
      tags$blockquote("About 610,000 people die of heart disease 
                                               in the United States every year–that's 1 in every 4 deaths"),  
      tags$div(
         HTML(paste(strong(tags$span(style="color:orange", "Data Source:",tags$p("CDC Wonder:"), tags$em("Diseases of Heart, ICD Code (100-109,l11,l13,l20-151) &"),
                                    tags$p("Fast Food Restaurant data from USDA-Food Environment Atlas"), sep = "")
                           )))
      ),
        tags$br(""),
                   selectInput("state", 
                            label = "Select a State", 
                            choices = states,
                            selected = 'Tennessee',selectize = FALSE),
                    selectInput("var", 
                             label = "Select a variable", 
                             choices = c("Heart Disease Mortality Rate" = 'Age_Adjusted_Rate',
                                         "Diabetes Percent in Persons age 20 and above" = "Adults_Diabetes_per_13",
                                         "Obesity Percent in Persons age 20 and above" = "Adult_Obese_Per_13"  ,
                                         "Percentage change in Recreational facilties 09 to 14"="Per_Rec_Fac_09-14",
                                         "Percentage of County Population 65 year old or older"="Per_65_Older_10",
                                         "Percentage of County Households with below poverty Threshold "="Poverty_rate_15"),
                              selectize = FALSE),
                    
      
      tags$br(""),
      
      tags$div(
        HTML(paste(tags$span(style="color:yellow", "NOTE:"), sep = ""))
      ),
      tags$p(HTML(paste(tags$span(strong(style="color:white", "Heart Disease mortality rate is age adjusted rate for the year 2017, Obesity and Diabetes are Age adjusted Percentage rates for the year 2013"), sep = "")))),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on County:"), sep = "")), "Info about Selected Variable and Fast Food Restaurants"),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on Circle:"), sep = "")), "Info about Heart Disease Mortality Rate and Fast Food Restaurants"),
      tags$p(HTML(paste(tags$span(style="color:yellow", "Hari Tanjore"), sep = "")),
             tags$br("")),
                selectInput("style", 
                         label = "Background Style for Map", 
                         choices = c("natural","white","cobalt", "albatross",
                              "watercolor","col_blind","beaver", "bw", 
                              "classic", "watercolor"),
                         selectize = FALSE)
      ),
     
     
     mainPanel(
       
              fluidRow(
               leafletOutput("FFR_tmap",width = "110%", height=400)),
              fluidRow(
                leafletOutput("FFR_TN", width = "110%", height=455))
    
      )
)
)
)


