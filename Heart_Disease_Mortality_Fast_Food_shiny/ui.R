


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("darkly"), 
  
  # Application title

  fluidRow(
    column(8, offset = 3, titlePanel("Heart Disease Mortality and Fast Food Restaurants Density in US"))),
  
  sidebarLayout(
    sidebarPanel(
      tags$blockquote("About 610,000 people die of heart disease 
                                               in the United States every year–that's 1 in every 4 deaths",
                      "Source: CDC WONDER","Data Source: USDA-Food Environment Atlas"),    
      # tags$br("Source: CDC WONDER"),
      # tags$br("Data Source: USDA-Food Environment Atlas"),
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 14px; }"))),width = 2,
       
                 selectInput("state", 
                            label = "Select a State", 
                            choices = states,
                            selected = 'State_name',selectize = FALSE)),
                
    
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

