
shinyUI(fluidPage(
  theme = shinytheme("darkly"), 
    fluidRow(
      column(8, offset = 3, titlePanel("Heart Disease Mortality and Fast Food Restaurants Density in US"))),
  
    sidebarLayout(
      sidebarPanel(width = 2,
      tags$blockquote("About 610,000 people die of heart disease 
                                               in the United States every year–that's 1 in every 4 deaths"),  
      tags$div(
         HTML(paste(strong(tags$span(style="color:orange", "Data Source:" ,tags$br("CDC Wonder,"),
                                    tags$p("USDA-Food Environment Atlas"), sep = "")
                           )))
      ),

                   selectInput("state", 
                            label = "Select a State", 
                            choices = states,
                            selected = 'State_name',selectize = FALSE),
                    selectInput("var", 
                             label = "Select a variable", 
                             choices = c("Age Adjusted Rate" = 'Age_Adjusted_Rate',
                                         "Percent Adult diabetes 2013" = "Adults_Diabetes_per_13",
                                         "Percent Adult Obese  2013" = "Adult_Obese_Per_13"  ,
                                         "Recreational facilties % 09 14"="Per_Rec_Fac_09-14",
                                         "Percent 65 Older 2010"="Per_65_Older_10",
                                         "Povery Rate 2015"="Poverty_rate_15"),
                              selectize = FALSE),
                    selectInput("style", 
                             label = "Select a Style for Background", 
                             choices = c("natural","white","cobalt", "albatross",
                                        "watercolor","col_blind","beaver", "bw", 
                                        "classic", "watercolor"),
                            selectize = FALSE),
      
      tags$br(""),
      tags$br(""),
      tags$div(
        HTML(paste(tags$span(style="color:yellow", "NOTE:"), sep = ""))
      ),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on County:"), sep = "")), "Info about Selected Variable"),
      tags$p(HTML(paste(tags$span(style="color:red", "Click on Circle:"), sep = "")), "Info about Heart Disease Mortality Rate and Fast Food Restaurants")
     
      ),
      
     
     
    
     
  
      mainPanel(
       
              fluidRow(
               leafletOutput("FFR_tmap",width = "125%")),
              fluidRow(
                leafletOutput("FFR_TN", width = "125%"))
    
      )
)
)
)


