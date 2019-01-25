
shinyUI(fluidPage(
   theme = shinytheme("darkly"), 
    fluidRow(
      column(8, offset = 3,titlePanel("Cardiovascular disease Mortality and Fast Food Restaurants Density in the US"))),
  
    sidebarLayout(
      sidebarPanel(width = 3,
          tags$div(
         HTML(paste(tags$span(style="font-size:16px;color:lightgrey", "World wide, around 17.9 million people die every year 
                      due to cardiovascular disease. In the United States, 610,000 people die of heart disease every year–that's 
                              1 in every 4 deaths. It affects both men and women. Risk Factors for Cardiovascular Disease include Diabetes,",
                              "Overweight and obesity,","Poor diet,","Physical inactivity,","&","Excessive alcohol use."),
                             tags$p("Data Source:"),tags$p("CDC Wonder:","Diseases of Heart,","ICD Code (100-109,l11,l13,l20-151)", 
                                                           "&","USDA-Food Environment Atlas"),
                                     sep = ""))
                           ),
      tags$hr(""),
                   selectInput("state", 
                            label = "Select a State", 
                            choices = states,
                            selected = 'Tennessee',selectize = FALSE),
                    selectInput("var", 
                             label = "Select a variable", 
                             choices = c("Cardiovascular Disease Mortality Rate" = "Age Adjusted Rate",
                                         "Diabetes Percent in Persons age 20 and above" = "Adult Diabetes Percent",
                                         "Obesity Percent in Persons age 20 and above" = "Adult Obese Percent"   ,
                                         "Percentage change in Recreational facilties 09 to 14"="Percent Rec Facilities 09 to 14",
                                         "Percentage of County Population 65 year old or older"="Percent 65 or Older",
                                         "Percentage of County Households with below poverty Threshold "="Poverty rate",
                                         "Percentage of Adults with Higschool or higher degree"= "Bachelors or Higher Degree"),
                              selectize = FALSE),
                  
                    
      tags$hr(""),
      tags$div(
        HTML(paste(tags$span(style="color:red", "NOTE:"), sep = ""))
      ),
      tags$p(HTML(paste(tags$span(style="color:yellow", "Cardiovascular disease mortality rate is age adjusted rate for the year 2017 per 100,000. 
                                  Obesity and Diabetes are Age adjusted Percentage rates for the year 2013."), sep = ""))),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on County:"), sep = "")), "Info about Selected Variable and Fast Food Restaurants(FFR)."),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on Circle:"), sep = "")), "Info about Heart Disease Mortality Rate and Fast Food Restaurants."),
      tags$i(HTML(paste(tags$span(style="color:lightgrey", "Hari Tanjore"), sep = "")),
             tags$br(""))),
                
       mainPanel(
         tabsetPanel(
           tabPanel(
             "US Counties Information", leafletOutput("FFR_TN",width = "110%", height=900)),
           tabPanel(
             "Overview in The US", withSpinner(type = 6, leafletOutput("FFR_tmap",width = "110%", height=900)))
          
    
      )
)
)
)
)






