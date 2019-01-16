
# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$FFR_tmap = renderLeaflet({
    
    var <- "Age_Adjusted_Rate"
    cuts <- c(75,175,275,375,475)
    #cuts <- c(0, 10, 20, 30, 40, 100)
    #mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")
    #cuts <- c(0.1, 0.3, 0.5, 1,3,4,6)
    
    tm <- tm_shape(FFR_shape_merge, projection = 2163) +
      tm_fill(var,
              breaks = cuts,
              palette = "BuPu",
              border.col = "white",
              title='Age Adjusted Rate',
              border.alpha = 0.5,
              id = "NAME",
              textNA = 'Unreliable',
              colorNA = "grey",
              popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                             "Percent Change in FFR from 09 to 14  / 1000 residents: " = "Percent_Change_FFR_2009_2014",
                             "Number of FastFood Resturants 09: " = "FFR09",
                             "Number of FastFood Resturants 14: " = "FFR14"))+
      tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
      tm_bubbles(size ="Percent_Change_FFR_2009_2014", col = "Percent_Change_FFR_2009_2014",
                 title.size="FFR % change 09 to 14",
                 title.col = "FFR % change 09 to 14",
                 breaks = c(-100, -75, -50, -25, -10,0, 10,25,50,100, 200,300),
                 palette = "-RdYlGn",
                 popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                                "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                                "Number of FastFood Resturants 09: " = "FFR09",
                                "Number of FastFood Resturants 14: " = "FFR14"))+
      
      tmap_options(max.categories = 14)+
      tm_shape(US_states) +
      tm_borders(lwd=2, col = "black", alpha = .25) +
      tm_layout(title = "",
                title.size = 1.1,
                
                title.position = c("center", "top"))+
      tm_legend(legend.position = c("right", "bottom"))+
      tm_style("natural")
    tmap_leaflet(tm)
    
    
  })
  
  output$FFR_TN = renderLeaflet({
    
    data <- FFR_shape_merge %>% 
      filter(State_name == input$state)
    
    var1 <- "Age_Adjusted_Rate"
    cuts1 <- c(100,120,180,200,300,350,400,450)
    tm1 <- tm_shape(data, projection = 2163) +
      tm_fill(var1,
              breaks = cuts1,
              palette = "BuPu",
              border.col = "black",
              title='Age Adjusted Rate',
              border.alpha = 0.5,
              id = "NAME",
              textNA = 'Unreliable',
              colorNA = "grey",
              popup.vars = c("County: " = "NAME", "Age Adjuste Mortality rate: " = "Age_Adjusted_Rate",
                             "Percent Change in FFR from 09 to 14 per 1000 residents: " = "Percent_Change_FFR_2009_2014",
                             "Number of FastFood Resturants 09: " = "FFR09",
                             "Number of FastFood Resturants 14: " = "FFR14"))+
      
      tm_polygons(col = var, alpha = 0.5, border.col = 'white')+
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
      tm_layout(title = "",
                title.size = 1.1,
                title.position = c("center", "top"))+
      tm_legend(legend.position = c("right", "bottom"))+
      tm_style("natural")
   
    tmap_leaflet(tm1)
    
  })
}